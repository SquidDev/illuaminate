open IlluaminateSemantics
open Html_options

(** Return the URL and class of a reference. *)
let reference_attrs ~options:{ resolve; _ } (reference : Reference.resolved) style =
  let link =
    match reference with
    | Internal { in_module; name; _ } ->
        Reference.section_of_name name
        |> Option.fold ~none:"" ~some:(fun x -> "#" ^ x)
        |> Printf.sprintf "module/%s.html%s" in_module
        |> resolve |> Option.some
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

let show_list ?(tag = "h3") title = function
  | [] -> Html.Default.nil
  | xs ->
      let open Html.Default in
      [ create_node ~tag ~children:[ str title ] ();
        create_node ~tag:"ul"
          ~children:(List.map (fun x -> create_node ~tag:"li" ~children:[ x ] ()) xs)
          ()
      ]
      |> many
