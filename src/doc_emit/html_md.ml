open Illuaminate.Html
open Cmarkit
module S = IlluaminateSemantics.Doc.Syntax
module A = IlluaminateSemantics.Doc.AbstractSyntax
module C = Cmarkit_renderer.Context
module H = Cmarkit_html

(** Parse a Markdown Extra-style attribute string. The leading/trailing braces should have already
    been stripped. *)
let parse_attributes s =
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
    ([], []) (String.split_on_char ' ' s)

(** Merge the classes into the main attributes, from a {! parse_attributes parsed attribute string}.
*)
let merge_classes ~classes ~attrs =
  match classes with
  | [] -> attrs
  | _ -> ("class", Some (String.concat " " classes)) :: attrs

let emit_attr c (k, v) =
  C.string c k;
  match v with
  | None -> ()
  | Some v -> C.string c {|="|}; H.html_escaped_string c v; C.string c {|"|}

let emit_attributes c attrs = List.iter (fun x -> C.string c " "; emit_attr c x) attrs

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

(** If [link] is a relative path, then resolve the file (relative to [md_file]) and
    {!Html_assets.find_asset copy it into the destination directory} *)
let resolve_asset_link ~md_file ~(options : Html_options.t) link =
  let uri = Uri.of_string link in
  match
    (Uri.scheme uri, Uri.userinfo uri, Uri.host uri, Uri.port uri, Uri.query uri, Uri.fragment uri)
  with
  | None, None, None, None, [], None -> (
    match Fpath.of_string (Uri.path uri) with
    | Ok path when Fpath.is_rel path ->
        let path = Fpath.(parent md_file // path |> normalize) in
        IlluaminateData.get options.data Html_assets.find_asset path |> Option.map options.resolve
    | _ -> None)
  | _ -> None

let emit_basic_image c ~link ~title ~text =
  let plain_text i =
    let lines = Inline.to_plain_text ~break_on_soft:false i in
    String.concat "\n" (List.map (String.concat "") lines)
  in
  let title =
    match title with
    | None -> ""
    | Some title -> String.concat "\n" (List.map (fun (_, (t, _)) -> t) title)
  in
  C.string c "<img src=\"";
  H.pct_encoded_string c link;
  C.string c "\" alt=\"";
  H.html_escaped_string c (plain_text text);
  C.byte c '\"';
  if title <> "" then (C.string c " title=\""; H.html_escaped_string c title; C.byte c '\"');
  C.string c "/>"

let emit_image ~path ~(options : Html_options.t) (c : Cmarkit_renderer.context) l =
  match (path, Inline.Link.reference_definition (C.get_defs c) l) with
  | Some md_file, Some (Link_definition.Def (ld, _)) -> (
    match
      Option.bind (Link_definition.dest ld) (fun (ld, _) -> resolve_asset_link ~md_file ~options ld)
    with
    | Some link ->
        emit_basic_image c ~link ~title:(Link_definition.title ld) ~text:(Inline.Link.text l);
        true
    | None -> false)
  | _ -> false

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
      String.sub extra 1 (l - 2) |> parse_attributes
    else ([], [])
  in

  match language with
  | "lua" ->
      let attrs = merge_classes ~classes ~attrs in
      let sink = Illuaminate.Output_sink.of_buffer (Cmarkit_renderer.Context.buffer c) in
      Illuaminate.Html.emit sink (Html_highlight.lua_block ~attrs ~options code)
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
          C.string c {|"|};
          emit_attributes c attrs);
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

module Stream = struct
  type t =
    { mutable index : int;
      mutable lines : Cmarkit.Block_line.t list
    }

  let stream lines =
    let state = { index = 0; lines } in
    let rec poll () =
      match state.lines with
      | [] -> None
      | (l, _) :: ls ->
          let index = state.index in
          if index >= String.length l then (
            state.index <- 0;
            state.lines <- ls;
            poll ())
          else (
            state.index <- index + 1;
            Some l.[index])
    in
    Markup.stream poll
end

let emit_block_html ~path ~options c block =
  match path with
  | None -> false
  | Some md_file ->
      let rec set_assoc k v = function
        | [] -> failwith "Did not find key"
        | (k', _) :: xs when k = k' -> (k, v) :: xs
        | kv :: xs -> kv :: set_assoc k v xs
      in
      let replace_asset_link ~attr name attrs e =
        match
          List.assoc_opt ("", attr) attrs
          |> CCOption.flat_map (resolve_asset_link ~md_file ~options)
        with
        | Some link -> `Start_element (name, set_assoc ("", attr) link attrs)
        | None -> e
      in
      let map_signal : Markup.signal -> Markup.signal = function
        | `Start_element (((ns, "img") as name), attrs) as e when ns = Markup.Ns.html ->
            replace_asset_link ~attr:"src" name attrs e
        | `Start_element (((ns, "source") as name), attrs) as e when ns = Markup.Ns.html ->
            replace_asset_link ~attr:"srcset" name attrs e
        | e -> e
      in
      Stream.stream block
      |> Markup.parse_html ~context:(`Fragment "body")
      |> Markup.signals |> Markup.map map_signal |> Markup.write_html
      |> Markup.iter (fun chr -> Buffer.add_char (C.buffer c) chr);
      true

let custom_html ~path ~options =
  let inline c = function
    | Inline.Link (l, _) -> emit_link ~options c l
    | Inline.Image (l, _) -> emit_image ~path ~options c l
    | _ -> false (* let the default HTML renderer handle that *)
  in
  let block c = function
    | Block.Code_block (block, _) -> emit_code_block ~options c block; true
    | Block.Block_quote (block, _) -> emit_block_quote c (Block.Block_quote.block block)
    | Block.Html_block (block, _) -> emit_block_html ~path ~options c block
    | _ -> false (* let the default HTML renderer handle that *)
  in
  Cmarkit_renderer.make ~inline ~block ()

let renderer ~path ~options =
  Cmarkit_renderer.compose (Cmarkit_html.renderer ~safe:false ()) (custom_html ~path ~options)

(** Render an inline fragment of a markdown document. *)
let render_inline ~path ~options ~doc inline =
  let b = Buffer.create 16 in
  let ctx = Cmarkit_renderer.Context.make (renderer ~path ~options) b in
  Cmarkit_renderer.Context.init ctx (S.Markdown.doc doc);
  Cmarkit_renderer.Context.inline ctx inline;
  Buffer.contents b |> raw

(** Render a markdown document to a HTML {!node}.*)
let md ?path ~options doc =
  S.Markdown.doc doc |> Cmarkit_renderer.doc_to_string (renderer ~path ~options) |> raw

(** Render a markdown document to a HTML {!node}, trying to unwrap documents containing a single
    paragraph. *)
let md_inline ~path ~options doc =
  match S.Markdown.as_single_paragraph doc with
  | Some t -> render_inline ~path ~options ~doc t
  | None -> md ?path ~options doc

let description_file (d : S.description) =
  let file = A.Comment_lines.span d.description_pos |> IlluaminateCore.Span.filename in
  file.path

let show_desc ~options = function
  | None -> nil
  | Some (d : S.description) -> md ?path:(description_file d) ~options d.description

let show_summary ~options = function
  | None -> nil
  | Some (d : S.description) ->
      Helpers.get_summary d.description
      |> render_inline ~path:(description_file d) ~options ~doc:d.description

let show_desc_inline ~options = function
  | None -> nil
  | Some (d : S.description) -> md_inline ~path:(description_file d) ~options d.description
