open IlluaminateSemantics
open Doc.Syntax
open Html.Default

let md ~helpers x =
  let open Omd in
  let code_style ~lang code =
    match lang with
    | "lua" -> Html_highlight.lua ~helpers code |> Format.asprintf "%a" emit
    | _ -> code
  in
  let preprocess node =
    match node with
    | Html ("illuaminate:ref", attrs, label) -> (
        let { link_reference; link_label = { description = label; _ }; link_style } =
          Link.of_tag attrs label
        in
        let link, classes = Html_basic.reference_attrs ~helpers link_reference link_style in
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
    | Code_block (lang, contents) -> (
      match lang with
      | "lua" ->
          Html_highlight.lua_block ~helpers contents |> Format.asprintf "%a" emit |> Option.some
      | _ ->
          str contents
          |> Format.asprintf "<pre class=\"highlight highlight-%s\">%a</pre>" lang emit
          |> Option.some )
    | _ -> None
  in
  x |> Omd_representation.visit preprocess |> Omd.to_html ~override:format ~cs:code_style |> raw

let rec md_inline ~helpers = function
  | [ Omd.Paragraph t ] -> md_inline ~helpers t
  | t -> md ~helpers t

let show_desc ~helpers = function
  | None -> nil
  | Some (d : description) -> md ~helpers d.description

let show_summary ~helpers = function
  | None -> nil
  | Some (d : description) -> Helpers.get_summary d.description |> md_inline ~helpers

let show_desc_inline ~helpers = function
  | None -> nil
  | Some (d : description) -> md_inline ~helpers d.description
