module Emitters = struct
  open Format

  let skip _ () = ()

  let attr out (k, v) = CCString.replace ~sub:"\"" ~by:"&quot;" v |> fprintf out " %s=\"%s\"" k

  let attrs = pp_print_list ~pp_sep:skip attr
end

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
    let attributes =
      attributes
      |> List.filter_map @@ function
         | _, None -> None
         | a, Some b -> Some (a, b)
    in
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
        (match s.[i] with
        | '"' -> Buffer.add_string b "&quot;"
        | '&' -> Buffer.add_string b "&amp;"
        | '<' -> Buffer.add_string b "&lt;"
        | '>' -> Buffer.add_string b "&gt;"
        | c -> Buffer.add_char b c);
        loop_escape b (i + 1))
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

  let do_emit ~indent =
    let open Format in
    let open Emitters in
    let cut out = if indent then pp_print_cut out () else () in
    let break out = if indent then pp_print_break out 0 2 else () in
    let open_box fmt = if indent then pp_open_hvbox fmt 0 else () in
    let close_box fmt = if indent then pp_close_box fmt () else () in
    let non_empty prev out go =
      if prev then cut out;
      go ();
      true
    in
    let rec go out prev = function
      | Many xs -> list prev out xs
      | Nil | Text "" | Raw "" -> prev
      | Raw x -> non_empty prev out @@ fun () -> pp_print_string out x
      | Text x -> non_empty prev out @@ fun () -> pp_print_string out (html_escape x)
      | Element
          { tag = ("br" | "img" | "link" | "meta") as tag; attributes; children = []; events = [] }
        ->
          non_empty prev out @@ fun () -> fprintf out "<%s%a />" tag attrs attributes
      | Element { tag; attributes; children; events = [] } ->
          non_empty prev out @@ fun () ->
          open_box out;
          fprintf out "<%s%a>%t%t%a%t%t</%s>" tag attrs attributes break open_box list' children
            close_box cut tag;
          close_box out
      | Element { events = _ :: _; _ } -> failwith "Cannot emit event handlers to a formatter."
    and list prev out = List.fold_left (go out) prev
    and list' out xs = list false out xs |> ignore in
    fun out node -> go out false node |> ignore

  let emit = do_emit ~indent:false

  let emit_pretty = do_emit ~indent:true

  let emit_doc out node =
    Format.fprintf out "<!DOCTYPE html>";
    emit out node
end

module Default = Make (struct
  type event_handler = |
end)
