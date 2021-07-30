open IlluaminateCore
open IlluaminateSemantics
open CCSexp
open CCFun

let field name value rest = list (atom name :: value) :: rest

let one x = [ x ]

let one' : t -> t list = function
  | `List xs -> xs
  | `Atom _ as x -> [ x ]

let atom' = one % atom

let list' = function
  | [ x ] -> x
  | xs -> list xs

let span = atom' % Span.show

let record = list % List.rev

let record' key xs = list (atom key :: List.rev xs)

let field_bool key value rest = if value then field key [] rest else rest

let field' name f value rest : t list =
  match value with
  | None -> rest
  | Some value -> field name (f value) rest

let spanned f (x : _ Span.spanned) = [] |> field "value" (f x.value) |> field "span" (span x.span)

let fields name f = List.fold_right (fun v rest -> field name (f v) rest)

module Make (X : sig
  module Doc : IlluaminateSemantics.Doc.AbstractSyntax.S

  val sexp : Doc.reference -> CCSexp.t
end) =
struct
  include X.Doc

  let reference = X.sexp

  let description (d : description) = Omd.to_markdown d.description |> atom'

  let rec type_ = function
    | Type.NilTy -> atom' "nil"
    | BoolTy x -> string_of_bool x |> atom'
    | IntTy x -> string_of_int x |> atom'
    | NumberTy x -> string_of_float x |> atom'
    | StringTy x -> Printf.sprintf "%S" x |> atom'
    | Named (x, _) -> [ reference x ]
    | Function _ -> atom' "function(...)"
    | Table _ -> atom' "{...}"
    | Union xs -> [ `List (atom "union" :: List.map (list' % type_) xs) ]

  let see { see_reference; see_label; see_description; see_span } =
    []
    |> field "ref" [ reference see_reference ]
    |> field "label" (atom' see_label)
    |> field "span" (span see_span)
    |> field' "description" description see_description
    |> record

  let deprecation { deprecation_message } =
    Option.fold ~none:[] ~some:description deprecation_message

  let position { path; start_line; end_line } =
    []
    |> field "path" (atom' path)
    |> field "start_line" (string_of_int start_line |> atom')
    |> field "end_line" (string_of_int end_line |> atom')
    |> List.rev

  let example = function
    | RawExample x -> spanned atom' x
    | RichExample x -> description x

  let opt_arg = function
    | Required -> Fun.id
    | Optional -> field "opt" []
    | Default x -> field "opt" [ atom x ]

  let arg { arg_name; arg_opt; arg_type; arg_description } =
    atom' "arg"
    |> field "name" (atom' arg_name)
    |> opt_arg arg_opt |> field' "type" type_ arg_type
    |> field' "description" description arg_description
    |> record

  let return { ret_type; ret_many; ret_description } =
    atom' "return" |> field' "type" type_ ret_type |> field_bool "many" ret_many
    |> field' "description" description ret_description
    |> record

  let change { change_kind; change_version; change_span; change_description } =
    let v_str =
      match change_kind with
      | Added -> "added"
      | Changed -> "changed"
    in
    atom' "change"
    |> field "kind" (atom' v_str)
    |> field "version" (atom' change_version)
    |> field "span" (span change_span)
    |> field' "description" description change_description
    |> record
end

module Comment = struct
  open Doc.Comment

  open Make (struct
    module Doc = Doc.Comment

    let sexp (Reference x) = atom x
  end)

  let to_sexp c =
    []
    |> field "source" (Span.show c.source |> atom')
    |> field' "description" description c.description
    |> fields "see" (one' % see) c.see
    |> fields "example" example c.examples
    |> field_bool "local" c.local
    |> fields "include" (spanned (one % reference)) c.includes
    |> field_bool "export" c.export
    |> field' "deprecated" deprecation c.deprecated
    |> field' "custom-source" position c.custom_source
    |> fields "changes" (one' % change) c.changes
    |> fields "args" (List.map arg) c.arguments
    |> fields "returns" (List.map return) c.returns
    |> fields "throws" description c.throws
    |> field' "module" (fun x -> atom' x.Span.value.mod_name) c.module_info
    |> field' "type" (fun x -> atom' x.type_name) c.type_info
    |> record
end

module Syntax = struct
  open Doc.Syntax

  open Make (struct
    module Doc = Doc.Syntax
    open! Reference

    let sexp = function
      | Internal { in_module; name; _ } ->
          []
          |> field "in-module"
               (atom' (Format.asprintf "%a" IlluaminateSemantics.Namespace.Ref.pp in_module))
          |> field' "name" atom' (section_of_name name)
          |> record
      | External { name; url } ->
          [] |> field "name" (atom' name) |> field' "url" atom' url |> record
      | Unknown x -> atom x
  end)

  let documented body (d : _ documented) =
    []
    |> field' "description" description d.description
    |> field "body" (body d.descriptor)
    |> fields "example" example d.examples
    |> fields "see" (one' % see) d.see
    |> field_bool "local" d.local |> field_bool "export" d.export
    |> field' "deprecated" deprecation d.deprecated
    |> field' "custom-source" position d.custom_source
    |> record

  let rec value = function
    | Function { args; rets; throws; has_self } ->
        []
        |> fields "args" (List.map arg) args
        |> fields "returns" (List.map return) rets
        |> fields "throws" description throws
        |> field_bool "self" has_self |> record
    | Table xs ->
        atom "table" :: List.map (fun (k, v) -> list [ atom k; documented (one' % value) v ]) xs
        |> list
    | Expr { ty; value } ->
        [] |> field "ty" (type_ ty) |> field' "value" atom' value |> record' "expr"
    | Type t -> atom "type" :: one' (type_info t) |> list
    | Unknown -> atom "unknown"
    | Undefined -> atom "undefined"

  and type_info t : t =
    []
    |> field "name" (atom' t.type_name)
    |> fields "member" (one' % member) t.type_members
    |> record

  and member x : t =
    []
    |> field "name" (atom' x.member_name)
    |> field_bool "method" x.member_is_method
    |> field "value" [ documented (one % value) x.member_value ]
    |> record

  let page_contents m (xs : t list) : t list =
    match m with
    | Module { mod_contents; mod_types; _ } ->
        xs
        |> field "contents" [ value mod_contents ]
        |> fields "type" (one' % documented (one' % type_info)) mod_types
    | Markdown -> xs

  let module_info m : t =
    []
    |> field "id" (atom' m.page_id)
    |> field "title" (atom' m.page_title)
    |> page_contents m.page_contents |> record
end
