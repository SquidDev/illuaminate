open IlluaminateSemantics
open Doc.Syntax
open Html.Default

let default_highlight attr label code =
  let class_name = if String.trim label = "" then None else Some ("highlight highlight-" ^ label) in
  create_node ~tag:"pre" ~attributes:(("class", class_name) :: attr)
    ~children:[ create_node ~tag:"code" ~children:[ str code ] () ]
    ()

let highlight ~options attrs lang code =
  let attrs = List.map (fun (k, v) -> (k, Some v)) attrs in
  let node =
    match lang with
    | "lua" -> Html_highlight.lua_block ~attrs ~options code
    | _ -> default_highlight attrs lang code
  in
  Format.asprintf "%a" emit node

let ref ~options link text =
  let kind, label =
    match text with
    | `Raw label -> (`Code, label)
    | `Desc label -> (`Text, label)
  in
  let link, classes = Html_basic.reference_attrs ~options link kind in
  let node =
    match link with
    | None ->
        create_node ~tag:"span" ~attributes:[ ("class", Some classes) ] ~children:[ str label ] ()
    | Some url ->
        create_node ~tag:"a"
          ~attributes:[ ("href", Some url); ("class", Some classes) ]
          ~children:[ str label ]
          ()
  in
  Format.asprintf "%a" emit node

let md ~options x = x |> Omd.to_html ~highlight:(highlight ~options) ~ref:(ref ~options) |> raw

let md_inline ~options = function
  | [ Omd.Paragraph (_, t) ] -> Omd.inline_to_html ~ref:(ref ~options) t |> raw
  | t -> md ~options t

let show_desc ~options = function
  | None -> nil
  | Some (d : description) -> md ~options d.description

let show_summary ~options = function
  | None -> nil
  | Some (d : description) ->
      Helpers.get_summary d.description |> Omd.inline_to_html ~ref:(ref ~options) |> raw

let show_desc_inline ~options = function
  | None -> nil
  | Some (d : description) -> md_inline ~options d.description
