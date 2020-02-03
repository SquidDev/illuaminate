open IlluaminateSemantics.Doc.Syntax
open Html.Default

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
    | Html ("illuaminate:ref", attrs, label) ->
        let tag, attrs =
          match attrs with
          | [ ("module", Some modu) ] ->
              ( "a",
                [ ("href", Some ("module/" ^ modu ^ ".html" |> resolve));
                  ("class", Some "reference")
                ] )
          | [ ("module", Some modu); ("sec", Some sec) ] ->
              ( "a",
                [ ("href", Some ("module/" ^ modu ^ ".html#sec:" ^ sec |> resolve));
                  ("class", Some "reference")
                ] )
          | [ ("href", Some href) ] -> ("a", [ ("href", Some href); ("class", Some "reference") ])
          | [] -> ("span", [ ("class", Some "reference") ])
          | _ -> ("span", [ ("class", Some "reference reference-unresolved") ])
        in
        Some [ Html (tag, attrs, label) ]
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
