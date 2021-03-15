open Linter
open IlluaminateCore
open IlluaminateSemantics
open! Doc.Syntax
module E = Doc.Extract

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "doc:unresolved-reference"

let check ~r ~span ~kind = function
  | Reference.Unknown x -> r.r ~span ~tag "Unknown %s %S." kind x
  | Internal _ | External _ -> ()

let rec type_ ~r ~span = function
  | Type.NilTy | BoolTy _ | IntTy _ | NumberTy _ | StringTy _ -> ()
  | Named (n, _) -> check ~r ~span ~kind:"type" n
  | Function { args; return = rs, rest } ->
      List.iter (fun { Type.ty; _ } -> type_ ~r ~span ty) args;
      List.iter (type_ ~r ~span) rs;
      Option.iter (type_ ~r ~span) rest
  | Table xs -> List.iter (table_entry ~r ~span) xs
  | Union xs -> List.iter (type_ ~r ~span) xs

and table_entry ~r ~span = function
  | Type.Field { value = x; optional = _; key = _ } | Item x | Many x -> type_ ~r ~span x
  | Hash { key; optional = _; value } -> type_ ~r ~span key; type_ ~r ~span value

let check_abstract ~r ~span =
  object
    val span = span

    inherit abstract_iter

    method! reference = check ~r ~span ~kind:"reference"

    method! type_ = type_ ~r ~span

    method! see { see_reference; see_label = _; see_span = span; see_description } =
      check ~r ~span ~kind:"reference" see_reference;
      Option.iter {<span>}#description see_description

    method! description { description; description_pos } =
      let span = Option.value ~default:span description_pos in
      let open Omd in
      let omd = function
        | Html ("illuaminate:ref", attrs, label) -> (
          match Link.of_tag attrs label with
          | { link_reference = Unknown x; _ } -> r.r ~span ~tag "Unknown reference %S." x
          | _ -> ())
        | _ -> ()
      in
      Doc.AbstractSyntax.Omd'.iter omd description
  end

let linter =
  make_no_opt ~tags:[ tag ]
    ~file:(fun () context r file ->
      match IlluaminateData.need context E.file file |> E.get_page with
      | None -> ()
      | Some m ->
          let iter = iter_of (check_abstract ~r) in
          iter#documented iter#page m)
    ()
