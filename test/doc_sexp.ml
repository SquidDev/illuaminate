open IlluaminateCore
open IlluaminateSemantics
open CCSexp
open CCFun

let field name value rest = list (atom name :: value) :: rest

let one x = [ x ]

let one' : t -> t list = function
  | `List xs -> xs
  | (`Atom _) as x -> [ x ]

let atom' = one % atom

let record = list % List.rev

let field' name f value rest : t list =
  match value with
  | None -> rest
  | Some value -> field name (f value) rest

let fields name f = List.fold_right (fun v rest -> field name (f v) rest)

module Make (X : sig
  module Doc : IlluaminateSemantics.Doc.AbstractSyntax.S

  val sexp : Doc.reference -> CCSexp.t
end) =
struct
  include X.Doc

  let reference = X.sexp

  let description (Description d) = Omd.to_markdown d |> atom'

  let rec type_ = function
    | Type.NilTy -> atom' "nil"
    | BoolTy x -> string_of_bool x |> atom'
    | IntTy x -> string_of_int x |> atom'
    | NumberTy x -> string_of_float x |> atom'
    | StringTy x -> Printf.sprintf "%S" x |> atom'
    | Named (x, _) -> [ reference x ]
    | Function _ -> atom' "function(...)"
    | Table _ -> atom' "{...}"
    | Union xs -> atom "union" :: List.map (record % type_) xs

  let see { see_reference; see_label; see_description } =
    []
    |> field "ref" [ reference see_reference ]
    |> field "label" (atom' see_label)
    |> field' "description" description see_description
    |> record

  let example = function
    | RawExample x -> atom' x
    | RichExample x -> description x

  let arg { arg_name; arg_opt; arg_type; arg_description } =
    []
    |> field "name" (atom' arg_name)
    |> field "opt" (string_of_bool arg_opt |> atom')
    |> field' "type" type_ arg_type
    |> field' "description" description arg_description
    |> record

  let return { ret_type; ret_many; ret_description } =
    [] |> field' "type" type_ ret_type
    |> field "many" (string_of_bool ret_many |> atom')
    |> field' "description" description ret_description
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
    |> field "local" (string_of_bool c.local |> atom')
    |> fields "include" (one % reference) c.includes
    |> fields "args" (List.map arg) c.arguments
    |> fields "returns" (List.map return) c.returns
    |> fields "throws" description c.throws
    |> field' "module" (fun x -> atom' x.mod_name) c.module_info
    |> field' "type" (fun x -> atom' x.type_name) c.type_info
    |> record
end
