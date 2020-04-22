open IlluaminateSemantics
open Doc.Syntax
open Html.Default

(** Return the URL and class of a reference. *)
let reference_attrs ~resolve (reference : Reference.resolved) style =
  let link =
    match reference with
    | Internal { in_module; name = Module; _ } -> Some (resolve ("module/" ^ in_module ^ ".html"))
    | Internal { in_module; name = Value v | Type v | Member (v, _); _ } ->
        Some (resolve ("module/" ^ in_module ^ ".html") ^ "#" ^ v)
    | External { url = Some url; _ } -> Some url
    | External { url = None; _ } -> None
    | Unknown _ -> None
  in
  let classes =
    match style with
    | `Text -> "reference reference-text"
    | `Code -> "reference reference-code"
  in
  let classes =
    match reference with
    | Unknown _ -> classes ^ " reference-unresolved"
    | _ -> classes
  in
  (link, classes)

let md ~resolve x =
  let open Omd in
  let highlight ~lang code =
    let code =
      match lang with
      | "lua" -> Html_highlight.lua code
      | _ -> str code
    in
    Format.asprintf "%a" emit code
  in
  let preprocess node =
    match node with
    | Html ("illuaminate:ref", attrs, label) -> (
        let { link_reference; link_label = Description label; link_style } =
          Link.of_tag attrs label
        in
        let link, classes = reference_attrs ~resolve link_reference link_style in
        match link with
        | None -> Some [ Html ("span", [ ("class", Some classes) ], label) ]
        | Some url -> Some [ Html ("a", [ ("href", Some url); ("class", Some classes) ], label) ] )
    | Html ("illuaminate:colour", [ ("colour", Some colour) ], label) ->
        Some
          [ Html
              ( "span",
                [ ("class", Some "colour") ],
                Html
                  ( "span",
                    [ ("class", Some "colour-ref");
                      ("style", Some ("background-color: #" ^ colour))
                    ],
                    [] )
                :: label )
          ]
    | _ -> None
  in
  let format node =
    match node with
    | Code_block (lang, contents) ->
        Some
          (Printf.sprintf "<pre class=\"highlight highlight-%s\">%s</pre>" lang
             (highlight ~lang contents))
    | _ -> None
  in
  x |> Omd_representation.visit preprocess |> Omd.to_html ~override:format ~cs:highlight |> raw

let rec md_inline ~resolve = function
  | [ Omd.Paragraph t ] -> md_inline ~resolve t
  | t -> md ~resolve t

let show_desc ~resolve = function
  | None -> nil
  | Some (Description x) -> md ~resolve x

let show_summary ~resolve = function
  | None -> nil
  | Some (Description x) -> Helpers.get_summary x |> md_inline ~resolve

let show_desc_inline ~resolve = function
  | None -> nil
  | Some (Description x) -> md_inline ~resolve x
