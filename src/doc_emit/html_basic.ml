open IlluaminateSemantics
open Html_options

(** Get the link this reference points to. *)
let reference_link ~options:{ resolve; _ } : Reference.resolved -> string option = function
  | Internal { in_module; name; _ } ->
      Helpers.reference_link in_module name |> resolve |> Option.some
  | External { url = Some url; _ } -> Some url
  | External { url = None; _ } -> None
  | Unknown _ -> None

let show_list ?(tag = "h3") ?(expandable = false) ?(expand = true) title =
  let open Illuaminate.Html in
  function
  | [] -> nil
  | xs ->
      [ create_node ~tag
          ~children:[ str title ]
          ~attributes:
            [ ("class", if expand then None else Some "collapsed");
              ("tabindex", if expandable then Some "0" else None)
            ]
          ();
        create_node ~tag:"ul"
          ~children:(List.map (fun x -> create_node ~tag:"li" ~children:[ x ] ()) xs)
          ()
      ]
      |> many
