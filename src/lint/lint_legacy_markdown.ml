open Linter
open IlluaminateCore
open IlluaminateSemantics
module C = Doc.Comment

let ldoc_reference = Error.Tag.make ~attr:[] ~level:Note "doc:ldoc-reference"
let docu_admonition = Error.Tag.make ~attr:[] ~level:Note "doc:docusaurus-admonition"

let description (r : _ reporter) ({ description; description_pos } : C.description) =
  let open Cmarkit in
  let module Cl = IlluaminateSemantics.Doc.AbstractSyntax.Comment_lines in
  let doc = C.Markdown.doc description in
  let pp_admonition out a =
    let level = Block.Admonition.level a |> fst |> Block.Admonition.level_name in
    match Block.Admonition.label a with
    | None -> Format.fprintf out "[!%s]" level
    | Some l ->
        let b = Buffer.create 16 in
        let c = Cmarkit_renderer.Context.make (Cmarkit_commonmark.renderer ()) b in
        Cmarkit_renderer.Context.init c doc;
        Cmarkit_renderer.Context.inline c l;
        Format.fprintf out "[%s][!%s]" (Buffer.contents b) level
  in

  let inline _ () = function
    | Inline.Link (l, meta) when Inline.Link.is_legacy_ref l ->
        let span =
          Cl.span_of_meta description_pos meta
          |> CCOption.get_lazy (fun () -> Cl.span description_pos)
        in
        let label = Inline.Link.referenced_label l |> Option.get |> Label.key in
        r.r ~span ~tag:ldoc_reference
          ~detail:(fun out -> Format.fprintf out "Replace with [%s]" label)
          "Use of LDoc reference syntax.";
        Folder.default
    | _ -> Folder.default
  in
  let block _ () = function
    | Block.Ext_admonition (a, meta) ->
        Format.printf "Admonition => %a\n@?" Cmarkit.Textloc.pp_dump (Cmarkit.Meta.textloc meta);
        let span =
          Cl.span_of_meta description_pos meta
          |> CCOption.get_lazy (fun () -> Cl.span description_pos)
        in
        r.r ~span ~tag:ldoc_reference
          ~detail:(fun out ->
            Format.fprintf out "Replace with a blockquote:@\n> %a\n> ..." pp_admonition a)
          "Use of Docusaurus admonition.";
        Folder.default
    | _ -> Folder.default
  in
  let folder = Folder.make ~inline ~block () in
  Folder.fold_doc folder () doc

let comment r (c : C.comment) =
  let description x = description r x in
  let example = function
    | C.RawExample _ -> ()
    | C.RichExample d -> description d
  in
  Option.iter description c.description;
  List.iter (fun x -> Option.iter description x.C.see_description) c.see;
  Option.iter (fun x -> Option.iter description x.C.deprecation_message) c.deprecated;
  List.iter (fun x -> Option.iter description x.C.change_description) c.changes;
  List.iter (fun x -> Option.iter description x.C.field_description) c.fields;
  List.iter (List.iter (fun x -> Option.iter description x.C.arg_description)) c.arguments;
  List.iter (List.iter (fun x -> Option.iter description x.C.ret_description)) c.returns;
  List.iter description c.throws;
  List.iter example c.examples

let file () context r _ =
  IlluaminateData.need context.data Doc.Parser.Data.file context.file
  |> Option.fold ~some:Doc.Parser.Data.comments ~none:[]
  |> List.iter (comment r)

let linter = make_no_opt ~tags:[ ldoc_reference; docu_admonition ] ~file ()
