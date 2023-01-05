open Linter
open IlluaminateCore
open IlluaminateSemantics
open! Doc.Syntax
module E = Doc.Extract
module NMap = Map.Make (Namespace)
module SMap = Map.Make (String)

let unresolved_tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "doc:unresolved-reference"
let local_tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "doc:local-reference"

type context =
  { data : IlluaminateData.context;
    report : File.t reporter;
    is_local : bool
  }

let check ~ctx ~span ~kind = function
  | Reference.Unknown x -> ctx.report.r ~span ~tag:unresolved_tag "Unknown %s %S." kind x
  | Internal { in_module; _ } when not ctx.is_local -> (
      let modu =
        IlluaminateData.need ctx.data E.all_pages ()
        |> NMap.find_opt in_module.namespace
        |> CCOption.flat_map (SMap.find_opt in_module.id)
      in
      match modu with
      | Some { local = true; _ } ->
          ctx.report.r ~span ~tag:local_tag
            "This %s references a private %S, and so cannot be referenced from public code." kind
            in_module.id
      | _ -> ())
  | Internal _ | External _ -> ()

let rec type_ ~ctx ~span = function
  | Type.NilTy | BoolTy _ | IntTy _ | NumberTy _ | StringTy _ -> ()
  | Named (n, _) -> check ~ctx ~span ~kind:"type" n
  | Function { args; return = rs, rest } ->
      List.iter (fun { Type.ty; _ } -> type_ ~ctx ~span ty) args;
      List.iter (type_ ~ctx ~span) rs;
      Option.iter (type_ ~ctx ~span) rest
  | Table xs -> List.iter (table_entry ~ctx ~span) xs
  | Union xs -> List.iter (type_ ~ctx ~span) xs

and table_entry ~ctx ~span = function
  | Type.Field { value = x; optional = _; key = _ } | Item x | Many x -> type_ ~ctx ~span x
  | Hash { key; optional = _; value } -> type_ ~ctx ~span key; type_ ~ctx ~span value

let check_abstract ~ctx ~span =
  object
    val span = span
    inherit abstract_iter
    method! reference = check ~ctx ~span ~kind:"reference"
    method! type_ = type_ ~ctx ~span

    method! see { see_reference; see_label = _; see_span = span; see_description } =
      check ~ctx ~span ~kind:"reference" see_reference;
      Option.iter {<span>}#description see_description

    method! description { description; description_pos } =
      let span = Option.value ~default:span description_pos in
      Doc.AbstractSyntax.Omd'.iter (check ~ctx ~span ~kind:"reference") description
  end

let linter =
  make_no_opt ~tags:[ unresolved_tag; local_tag ]
    ~file:(fun () data report file ->
      match IlluaminateData.need data E.file file |> E.get_page with
      | None -> ()
      | Some m ->
          let iter = iter_of (check_abstract ~ctx:{ data; report; is_local = m.local }) in
          iter#documented iter#page m)
    ()
