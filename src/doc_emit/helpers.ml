open IlluaminateSemantics
open Doc
open Doc.AbstractSyntax
open Doc.Syntax

(** Try to extract a summary from a markdown document. This will take the first sentence or line of
    the document, or at most [[max_length]] characters. *)
let get_summary ?(max_length = 120) (Syntax.Markdown.Markdown desc) : Cmarkit.Inline.t =
  let module I = Cmarkit.Inline in
  let module B = Cmarkit.Block in
  let single_space = I.Text (" ", Cmarkit.Meta.none) in
  let empty = I.Text ("", Cmarkit.Meta.none) in
  let rec go space : I.t -> int * I.t = function
    | _ when space <= 0 -> (0, I.empty)
    | I.Inlines (xs, a) ->
        let space, xs = go_list space [] xs in
        (space, I.Inlines (xs, a))
    | I.Text (t, a) -> (
        let sentence_end =
          [ '.'; '!'; '?' ]
          |> List.map (String.index_opt t)
          |> List.fold_left
               (fun exist idx ->
                 match (exist, idx) with
                 | Some exist, Some idx when idx < exist -> Some idx
                 | None, idx -> idx
                 | exist, _ -> exist)
               None
        in
        match sentence_end with
        | Some i -> (0, I.Text (CCString.take (i + 1) t, a))
        | None when String.length t >= space -> (0, I.Text (CCString.take space t ^ "...", a))
        | None -> (space - String.length t, I.Text (t, a)))
    (* Basic formatting blocks *)
    | I.Emphasis (em, a) ->
        on_child
          (fun body -> I.Emphasis (I.Emphasis.make ~delim:(I.Emphasis.delim em) body, a))
          space (I.Emphasis.inline em)
    | I.Strong_emphasis (em, a) ->
        on_child
          (fun body -> I.Strong_emphasis (I.Emphasis.make ~delim:(I.Emphasis.delim em) body, a))
          space (I.Emphasis.inline em)
    | I.Link (link, a) ->
        on_child
          (fun body -> I.Link (I.Link.make body (I.Link.reference link), a))
          space (I.Link.text link)
    | I.Code_span (code, _) as inline -> (space - String.length (I.Code_span.code code), inline)
    | I.Ext_colour (code, _) as inline -> (space - String.length code, inline)
    | I.Break _ -> (space - 1, single_space)
    | I.Image _ -> (space, empty)
    | _ -> failwith ("Inline not yet handled " ^ Cmarkit_commonmark.of_doc desc)
  and on_child factory space node =
    let space, node = go space node in
    (space, factory node)
  and go_list space ys = function
    | [] -> (space, List.rev ys)
    | _ when space <= 0 -> (space, List.rev ys)
    | x :: xs ->
        let space, x = go space x in
        go_list space (x :: ys) xs
  in
  let rec block = function
    | B.Paragraph (x, _) -> B.Paragraph.inline x |> go max_length |> snd |> Option.some
    | B.Heading (x, _) -> B.Heading.inline x |> go max_length |> snd |> Option.some
    | B.Blocks (xs, _) -> List.find_map block xs
    | B.Html_block (start :: [], _)
      when String.starts_with ~prefix:"<!--" (Cmarkit.Block_line.to_string start) -> None
    | Cmarkit.Block.Link_reference_definition _ | Cmarkit.Block.Blank_line _ -> None
    | _ -> Some empty
  in
  Cmarkit.Doc.block desc |> block |> Option.value ~default:empty

(** Get a summary as plain text. *)
let get_summary_as_text ?max_length (desc : description) =
  get_summary ?max_length desc.description |> Cmarkit_ext.inline_text |> String.trim

(** Get a link to a node. *)
let link ~source_link { definition; custom_source; _ } =
  let link =
    match custom_source with
    | None -> Span definition
    | Some pos -> Position pos
  in
  source_link link

let reference_link { Namespace.Ref.namespace = Namespace ns; id; _ } ref =
  let section = Reference.section_of_name ref in
  match section with
  | None -> Format.asprintf "%s/%s.html" ns id
  | Some sec -> Format.asprintf "%s/%s.html#%s" ns id sec
