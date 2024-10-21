type 'ev node =
  | Element of
      { tag : string;
        attributes : (string * string) list;
        events : (string * 'ev) list;
        children : 'ev node list
      }
  | Text of string
  | Raw of string
  | Nil
  | Many of 'ev node list

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

type no_events = |
type node_ = no_events node

let rec emit_attr_value out str start len =
  if start > len then ()
  else
    match String.index_from_opt str start '"' with
    | Some i ->
        Output_sink.write_substring out str start (i - start);
        Output_sink.write out "&quot;";
        emit_attr_value out str (i + 1) len
    | None -> Output_sink.write_substring out str start (len - start)

let emit_attr out (k, v) =
  Output_sink.write out " ";
  Output_sink.write out k;
  Output_sink.write out "=\"";
  emit_attr_value out v 0 (String.length v);
  Output_sink.write out "\""

let emit_attrs out xs = List.iter (emit_attr out) xs

let do_emit ~indent =
  let open Format in
  let cut out = pp_print_cut out () in
  let break out = if indent then pp_print_break out 0 2 else () in
  let open_box fmt = if indent then pp_open_hvbox fmt 0 else () in
  let close_box fmt = if indent then pp_close_box fmt () else () in
  let non_empty prev out go =
    if prev then cut out;
    go ();
    true
  in
  let attrs out attrs = emit_attrs (Output_sink.of_formatter out) attrs in
  let rec go out prev : node_ -> _ = function
    | Many xs -> list prev out xs
    | Nil | Text "" | Raw "" -> prev
    | Raw x -> non_empty prev out @@ fun () -> pp_print_string out x
    | Text x -> non_empty prev out @@ fun () -> pp_print_string out (html_escape x)
    | Element
        { tag = ("br" | "img" | "link" | "meta") as tag; attributes; children = []; events = [] } ->
        non_empty prev out @@ fun () -> fprintf out "<%s%a />" tag attrs attributes
    | Element { tag; attributes; children; events = [] } ->
        non_empty prev out @@ fun () ->
        open_box out;
        fprintf out "<%s%a>%t%t%a%t%t</%s>" tag attrs attributes break open_box list' children
          close_box cut tag;
        close_box out
    | Element { events = _ :: _; _ } -> .
  and list prev out = List.fold_left (go out) prev
  and list' out xs = list false out xs |> ignore in
  fun out node -> go out false node |> ignore

let emit_pretty = do_emit ~indent:true

let rec emit out : node_ -> unit = function
  | Nil | Text "" | Raw "" -> ()
  | Many xs -> List.iter (emit out) xs
  | Raw txt -> Output_sink.write out txt
  | Text txt -> Output_sink.write out (html_escape txt)
  | Element
      { tag = ("br" | "img" | "link" | "meta") as tag; attributes; children = []; events = [] } ->
      Output_sink.write out "<";
      Output_sink.write out tag;
      emit_attrs out attributes;
      Output_sink.write out " />"
  | Element { tag; attributes; children; events = [] } ->
      Output_sink.write out "<";
      Output_sink.write out tag;
      emit_attrs out attributes;
      Output_sink.write out ">";
      List.iter (emit out) children;
      Output_sink.write out "</";
      Output_sink.write out tag;
      Output_sink.write out ">"
  | Element { events = _ :: _; _ } -> .

let emit_doc out node =
  Output_sink.write out "<!DOCTYPE html>";
  emit out node
