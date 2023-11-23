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
            "This references a private module %S, and so cannot be referenced from public code."
            in_module.id
      | _ -> ())
  | Internal _ | External _ -> ()

let rec type_ ~ctx ~span = function
  | Type.NilTy | BoolTy _ | IntTy _ | NumberTy _ | StringTy _ -> ()
  | Named { ref; _ } -> check ~ctx ~span ~kind:"type" ref
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
      Doc.Syntax.Markdown.iter_references
        (fun label refr ->
          let span =
            Cmarkit.Label.meta label
            |> Doc.AbstractSyntax.Comment_lines.span_of_meta description_pos
            |> CCOption.get_lazy (fun () -> Doc.AbstractSyntax.Comment_lines.span description_pos)
          in
          check ~ctx ~span ~kind:"reference" refr)
        description
  end

let linter =
  make_no_opt ~tags:[ unresolved_tag; local_tag ]
    ~file:(fun () context report _ ->
      match IlluaminateData.need context.data E.file context.file |> Option.get |> E.get_page with
      | None -> ()
      | Some m ->
          let iter =
            iter_of (check_abstract ~ctx:{ data = context.data; report; is_local = m.local })
          in
          iter#documented iter#page m)
    ()
