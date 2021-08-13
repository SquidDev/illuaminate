open IlluaminateCore
open Doc_syntax

open struct
  module C = Doc_comment
end

module Tag = struct
  let type_mismatch = Error.Tag.make ~attr:[ Default ] ~level:Warning "doc:type-mismatch"

  let kind_mismatch = Error.Tag.make ~attr:[ Default ] ~level:Error "doc:kind-mismatch"

  let all = [ kind_mismatch; type_mismatch ]
end

module Value = struct
  module Lift = Doc_abstract_syntax.Lift (Doc_comment) (Doc_syntax)

  let mk_ref (Reference.Reference x) = Reference.Unknown x

  let lift : Lift.t = { any_ref = mk_ref; type_ref = mk_ref }

  let debug_name = function
    | Function _ as x -> "function" ^ get_suffix x
    | Expr _ as e -> "?" ^ get_suffix e
    | Table _ -> "table"
    | Type _ -> "type"
    | Unknown -> "unknown"
    | Undefined -> "undefined"

  let get_function : C.comment -> value option = function
    | { arguments = []; returns = []; throws = []; _ } -> None
    | { arguments; returns; throws; _ } ->
        let args = List.map (List.map (Lift.arg lift)) arguments
        and rets = List.map (List.map (Lift.return lift)) returns
        and throws = List.map (Lift.description lift) throws in
        Some (Function { args; rets; throws; has_self = false })

  let get_value ~report (comment : C.comment) =
    match (get_function comment, comment.type_info) with
    | None, None -> Unknown
    | Some x, None -> x
    | None, Some { type_name } -> Type { type_name; type_members = [] }
    | Some _, Some _ ->
        report Tag.kind_mismatch comment.source "Term is marked as both a function and a type";
        Undefined

  let get_documented ~report (comment : C.comment) =
    { description = Option.map (Lift.description lift) comment.description;
      definition = comment.source;
      descriptor = get_value ~report comment;
      examples = List.map (Lift.example lift) comment.examples;
      see = List.map (Lift.see lift) comment.see;
      local = comment.local;
      export = comment.export;
      deprecated = Option.map (Lift.deprecation lift) comment.deprecated;
      custom_source = comment.custom_source;
      changes = List.map (Lift.change lift) comment.changes
    }
end

module Merge = struct
  let documented (merge : Span.t -> 'a -> 'b -> 'c) (implicit : 'a documented)
      (explicit : 'b documented) =
    { description = CCOpt.or_ ~else_:explicit.description implicit.description;
      definition = implicit.definition;
      descriptor = merge implicit.definition implicit.descriptor explicit.descriptor;
      examples = implicit.examples @ explicit.examples;
      see = implicit.see @ explicit.see;
      local = implicit.local || explicit.local;
      export = implicit.export || explicit.export;
      deprecated = CCOpt.or_ ~else_:explicit.deprecated implicit.deprecated;
      custom_source = CCOpt.or_ ~else_:explicit.custom_source implicit.custom_source;
      changes = (if CCList.is_empty implicit.changes then explicit.changes else implicit.changes)
    }

  (** Right biased union of two values. *)
  let value ~errs pos left right =
    match (left, right) with
    (* The trivial cases *)
    | Unknown, x | x, Unknown -> x
    | Undefined, _ | _, Undefined -> Undefined
    | Table x, Table y -> Table (x @ y) (* TODO: Merge keys *)
    | Function _, Function _ -> right (* TODO: Validate matching args *)
    | Expr { ty; value }, Expr other when ty = other.ty -> (
      (* Expressions of the same type are merged. We really need a better strategy for this - it's
         only designed to detect constants. *)
      match (value, other.value) with
      | Some v, Some ov when v = ov -> Expr { value; ty }
      | _ -> Expr { ty; value = None })
    | Expr { ty = NilTy; _ }, x | x, Expr { ty = NilTy; _ } ->
        (* If someone has assigned to nil and something else, prioritise that definition. *)
        x
    | Table fields, Type { type_name; type_members }
    | Type { type_name; type_members }, Table fields ->
        (* If we've an index metafield, use that instead *)
        let fields =
          match List.assoc_opt "__index" fields with
          | Some { descriptor = Table fields; _ } -> fields
          | _ -> fields
        in
        Type
          { type_name;
            type_members =
              type_members
              @ (fields
                |> List.map (fun (member_name, value) ->
                       let member_value, member_is_method =
                         match value.descriptor with
                         | Function { has_self = true; _ } -> (value, true)
                         | Function ({ args = [ ({ arg_name = "self"; _ } :: args) ]; _ } as f) ->
                             let v = Function { f with args = [ args ]; has_self = true } in
                             ({ value with descriptor = v }, true)
                         | _ -> (value, false)
                       in
                       { member_name; member_is_method; member_value }))
          }
    | _ ->
        Printf.sprintf "Conflicting definitions, cannot merge `%s` and `%s`" (Value.debug_name left)
          (Value.debug_name right)
        |> Error.report errs Tag.kind_mismatch pos;
        right

  (** Merge two documented values. *)
  let doc_value ~errs = documented (value ~errs)

  let page_contents ~errs span left right =
    match (left, right) with
    | _, Markdown | Markdown, _ ->
        left (* TODO: Error. Markdown documents should never be merged. *)
    | Module left, Module right ->
        Module
          { mod_kind = left.mod_kind;
            mod_types = left.mod_types @ right.mod_types;
            mod_contents = value ~errs span left.mod_contents right.mod_contents
          }

  let page ~errs span left right =
    { page_id = left.page_id;
      page_title = left.page_title;
      page_namespace = left.page_namespace;
      page_contents = page_contents ~errs span left.page_contents right.page_contents
    }
end

module DropLocal = struct
  let rec value : value -> value = function
    | Table xs ->
        Table
          (List.filter_map
             (fun (k, v) ->
               if v.local then None else Some (k, { v with descriptor = value v.descriptor }))
             xs)
    | Type t -> Type (type_info t)
    | (Function _ | Expr _ | Unknown | Undefined) as v -> v

  and type_info { type_name; type_members } : type_info =
    { type_name;
      type_members =
        List.filter_map
          (fun ({ member_value = v; _ } as m) ->
            if v.local then None
            else Some { m with member_value = { v with descriptor = v.descriptor } })
          type_members
    }

  let mod_types =
    List.filter_map (fun v ->
        if v.local then None else Some { v with descriptor = type_info v.descriptor })
end
