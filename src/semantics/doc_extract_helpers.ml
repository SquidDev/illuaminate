open IlluaminateCore
open Doc_syntax

open struct
  module C = Doc_comment
end

type extract_error =
  | Value_mismatch of Span.t * value * value
  | Func_and_type of Span.t
  | Func_and_field of Span.t

module Value = struct
  module Lift = Doc_abstract_syntax.Lift (Doc_comment) (Doc_syntax)

  let lift : Lift.t =
    { any_ref = (fun (Reference x) -> (None, Unknown x));
      type_ref = (fun (Reference x) -> Unknown x)
    }

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

  let field_value (field : C.field) : value documented =
    let descriptor =
      match field.field_type with
      | None -> Unknown
      | Some ty -> Expr { ty = Lift.ty lift ty; value = None }
    in
    { description = Option.map (Lift.description lift) field.field_description;
      descriptor;
      definition = field.field_pos;
      examples = [];
      see = [];
      local = false;
      export = false;
      deprecated = None;
      custom_source = None;
      changes = []
    }

  let field_member (field : C.field) =
    { member_name = field.field_name; member_is_method = false; member_value = field_value field }

  let get_value ~report (comment : C.comment) =
    match (get_function comment, comment.type_info, comment.fields) with
    | None, None, [] -> Unknown
    | Some x, None, [] -> x
    | None, Some { type_name }, fields ->
        Type { type_name; type_members = List.map field_member fields }
    | None, None, (_ :: _ as fields) ->
        Table (List.map (fun f -> (f.C.field_name, field_value f)) fields)
    | Some _, Some _, _ ->
        report (Func_and_type comment.source);
        Undefined
    | Some _, None, _ ->
        report (Func_and_field comment.source);
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
    { description = CCOption.or_ ~else_:explicit.description implicit.description;
      definition = implicit.definition;
      descriptor = merge implicit.definition implicit.descriptor explicit.descriptor;
      examples = implicit.examples @ explicit.examples;
      see = implicit.see @ explicit.see;
      local = implicit.local || explicit.local;
      export = implicit.export || explicit.export;
      deprecated = CCOption.or_ ~else_:explicit.deprecated implicit.deprecated;
      custom_source = CCOption.or_ ~else_:explicit.custom_source implicit.custom_source;
      changes = (if CCList.is_empty implicit.changes then explicit.changes else implicit.changes)
    }

  (** Right biased union of two values. *)
  let value ~report pos left right =
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
                         | Function ({ args = [ { arg_name = "self"; _ } :: args ]; _ } as f) ->
                             let v = Function { f with args = [ args ]; has_self = true } in
                             ({ value with descriptor = v }, true)
                         | _ -> (value, false)
                       in
                       { member_name; member_is_method; member_value }))
          }
    | _ ->
        report (Value_mismatch (pos, left, right));
        right

  (** Merge two documented values. *)
  let doc_value ~report = documented (value ~report)

  let page_value ~report span left right =
    match (left, right) with
    | _, None | None, _ -> left (* TODO: Error. Markdown documents should never be merged. *)
    | Some left, Some right -> Some (value ~report span left right)

  let page ~report span left right =
    { page_ref = left.page_ref;
      page_value = page_value ~report span left.page_value right.page_value;
      page_types = left.page_types @ right.page_types;
      page_module_kind = left.page_module_kind
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
