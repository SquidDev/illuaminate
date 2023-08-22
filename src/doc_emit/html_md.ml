open Html.Default
open Cmarkit
module S = IlluaminateSemantics.Doc.Syntax
module A = IlluaminateSemantics.Doc.AbstractSyntax
module C = Cmarkit_renderer.Context
module H = Cmarkit_html

let emit_reference_link ~options c ~target l =
  match Html_basic.reference_link ~options target with
  | None ->
      C.string c {|<span class="reference reference-unresolved">|};
      C.inline c (Inline.Link.text l);
      C.string c {|</span>|}
  | Some link ->
      C.string c {|<a href="|};
      H.pct_encoded_string c link;
      C.string c {|" class="reference">|};
      C.inline c (Inline.Link.text l);
      C.string c {|</a>|}

let emit_invalid_admonition c l =
  C.string c "[";
  C.inline c (Inline.Link.text l);
  C.string c "][]";
  C.string c "<-- Misplaced admonition reference -->"

let emit_link ~options c l =
  match Inline.Link.referenced_label l with
  | None -> false
  | Some label -> (
      let meta = Label.meta label in
      match Meta.find S.Markdown.reference meta with
      | Some target ->
          emit_reference_link ~options c ~target l;
          true
      | None -> (
        match Meta.find A.Cmarkit_meta.admonition_level meta with
        | Some _ -> emit_invalid_admonition c l; true
        | None -> false))

let emit_code_block ~options c block =
  let language, extra =
    Block.Code_block.info_string block
    |> CCOption.flat_map (fun (x, _) -> Block.Code_block.language_of_info_string x)
    |> Option.value ~default:("", "")
  in
  let code = Block.Code_block.code block |> Cmarkit_ext.block_lines_contents in

  (* If the remaining text is of the form {...}, treat that as a Pandoc-style attribute string. *)
  let classes, attrs =
    let l = String.length extra in
    if l > 2 && extra.[0] = '{' && extra.[l - 1] == '}' then
      let attr_str = String.sub extra 1 (l - 2) |> String.split_on_char ' ' in
      List.fold_left
        (fun (classes, extra) s ->
          if String.length s = 0 then (classes, extra)
          else
            match s.[0] with
            | '.' -> (CCString.drop 1 s :: classes, extra)
            | '#' -> (classes, ("id", Some (CCString.drop 1 s)) :: extra)
            | _ -> (
              match String.index_opt s '=' with
              | None -> (classes, (s, None) :: extra)
              | Some i -> (classes, (CCString.take i s, Some (CCString.drop (i + 1) s)) :: extra)))
        ([], []) attr_str
    else ([], [])
  in

  match language with
  | "lua" ->
      let attrs =
        match classes with
        | [] -> attrs
        | _ -> ("classes", Some (String.concat " " classes)) :: attrs
      in
      Cmarkit_ext.cprintf c "%a" Html.Default.emit (Html_highlight.lua_block ~attrs ~options code)
  | _ ->
      C.string c {|<pre|};
      (match (language, classes) with
      | "", [] -> ()
      | "", class_ :: classes ->
          C.string c {| class="|};
          C.string c class_;
          List.iter (fun s -> C.string c " "; C.string c s) classes;
          C.string c {|"|}
      | _, _ ->
          C.string c {| class="highlight language-|};
          C.string c language;
          List.iter (fun s -> C.string c " "; C.string c s) classes;
          C.string c {|"|});
      C.string c {|>|};
      Cmarkit_html.html_escaped_string c code;
      C.string c {|</pre>|}

let emit_block_quote c body =
  let ( let* ) = Option.bind in
  let admonition =
    let* first, rest =
      match body with
      | Block.Paragraph (p, _) -> Some (p, [])
      | Block.Blocks (Block.Paragraph (p, _) :: xs, _) -> Some (p, xs)
      | _ -> None
    in
    let* link, first_rest =
      match Block.Paragraph.inline first with
      | Inline.Link (l, _) -> Some (l, [])
      | Inline.Inlines (Inline.Link (l, _) :: Inline.Break _ :: rest, _) -> Some (l, rest)
      | _ -> None
    in
    let* desc, refr =
      match Inline.Link.reference link with
      | `Ref (`Full, _, l) -> Some (Some (Inline.Link.text link), l)
      | `Ref ((`Collapsed | `Shortcut), _, l) -> Some (None, l)
      | `Inline _ -> None
    in
    let* level = Label.meta refr |> Meta.find A.Cmarkit_meta.admonition_level in

    let first = Block.Paragraph.make (Inline.Inlines (first_rest, Meta.none)) in
    Some (level, desc, Block.Blocks (Block.Paragraph (first, Meta.none) :: rest, Meta.none))
  in
  match admonition with
  | None -> false
  | Some (level, label, contents) ->
      H.admonition c ~level ?label contents;
      true

let custom_html ~options =
  let inline c = function
    | Inline.Link (l, _) -> emit_link ~options c l
    | _ -> false (* let the default HTML renderer handle that *)
  in
  let block c = function
    | Block.Code_block (block, _) -> emit_code_block ~options c block; true
    | Block.Block_quote (block, _) -> emit_block_quote c (Block.Block_quote.block block)
    | _ -> false (* let the default HTML renderer handle that *)
  in
  Cmarkit_renderer.make ~inline ~block ()

let renderer ~options =
  Cmarkit_renderer.compose (Cmarkit_html.renderer ~safe:false ()) (custom_html ~options)

(** Render an inline fragment of a markdown document. *)
let render_inline ~options ~doc inline =
  let b = Buffer.create 16 in
  let ctx = Cmarkit_renderer.Context.make (renderer ~options) b in
  Cmarkit_renderer.Context.init ctx (S.Markdown.doc doc);
  Cmarkit_renderer.Context.inline ctx inline;
  Buffer.contents b |> raw

(** Render a markdown document to a HTML {!node}.*)
let md ~options doc =
  S.Markdown.doc doc |> Cmarkit_renderer.doc_to_string (renderer ~options) |> raw

(** Render a markdown document to a HTML {!node}, trying to unwrap documents containing a single
    paragraph. *)
let md_inline ~options doc =
  match S.Markdown.as_single_paragraph doc with
  | Some t -> render_inline ~options ~doc t
  | None -> md ~options doc

let show_desc ~options = function
  | None -> nil
  | Some (d : S.description) -> md ~options d.description

let show_summary ~options = function
  | None -> nil
  | Some (d : S.description) ->
      Helpers.get_summary d.description |> render_inline ~options ~doc:d.description

let show_desc_inline ~options = function
  | None -> nil
  | Some (d : S.description) -> md_inline ~options d.description
