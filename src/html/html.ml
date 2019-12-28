module Make (X : sig
  type event_handler
end) =
struct
  type event_handler = X.event_handler

  type node =
    | Element of
        { tag : string;
          attributes : (string * string) list;
          events : (string * event_handler) list;
          children : node list
        }
    | Text of string
    | Raw of string
    | Nil
    | Many of node list

  let create_node ~tag ?(attributes = []) ?(events = []) ?(children = []) () =
    Element { tag; attributes; events; children }

  let str x = Text x

  let raw x = Raw x

  let nil = Nil

  let many xs = Many xs

  let html_escape s =
    let n = String.length s in
    let rec loop_escape b i =
      if i >= n then Buffer.contents b
      else (
        ( match s.[i] with
        | '"' -> Buffer.add_string b "&quot;"
        | '&' -> Buffer.add_string b "&amp;"
        | '<' -> Buffer.add_string b "&lt;"
        | '>' -> Buffer.add_string b "&gt;"
        | c -> Buffer.add_char b c );
        loop_escape b (i + 1) )
    in
    let rec loop_pure i =
      if i >= n then s
      else
        match s.[i] with
        | '"' | '&' | '<' | '>' ->
            let b = Buffer.create n in
            Buffer.add_substring b s 0 i; loop_escape b i
        | _ -> loop_pure (i + 1)
    in
    loop_pure 0

  let emit out node =
    let rec go = function
      | Many xs -> List.iter go xs
      | Nil -> ()
      | Raw x -> output_string out x
      | Text x -> output_string out (html_escape x)
      | Element { tag = ("br" | "img" | "meta") as tag; attributes; children = []; events = [] }
        -> (
        match attributes with
        | [] -> Printf.fprintf out "<%s />" tag
        | _ ->
            Printf.fprintf out "<%s" tag;
            attributes |> List.iter (fun (k, v) -> Printf.fprintf out " %s=\"%s\"" k v);
            output_string out " />" )
      | Element { tag; attributes; children; events = [] } ->
          ( match attributes with
          | [] -> Printf.fprintf out "<%s>" tag
          | _ ->
              Printf.fprintf out "<%s" tag;
              attributes |> List.iter (fun (k, v) -> Printf.fprintf out " %s=\"%s\"" k v);
              output_string out ">" );
          List.iter go children; Printf.fprintf out "</%s>" tag
      | Element { events = _ :: _; _ } -> failwith "Cannot emit event handlers to a formatter."
    in
    output_string out "<!DOCTYPE html>";
    go node
end

module Default = Make (struct
  type event_handler = |
end)
