module StringMap = Map.Make (String)

type attribute =
  | Default
  | Unused
  | Deprecated

type level =
  | Critical
  | Error
  | Warning
  | Note

module Tag = struct
  type t =
    { name : string;
      level : level;
      attributes : attribute list
    }

  let pp f { name; _ } = Format.fprintf f "%s" name
  let tags = ref StringMap.empty

  let make ~attr ~level name =
    let tag = { level; name; attributes = attr } in
    tags := StringMap.add name tag !tags;
    tag

  let find name = StringMap.find_opt name !tags
  let compare l r = String.compare l.name r.name
  let has a l = List.mem a l.attributes

  type filter = t -> bool
end

module Error = struct
  type t =
    { tag : Tag.t;
      span : Span.t;
      message : string;
      details : (Format.formatter -> unit) option
    }

  let span_compare { span = a; _ } { span = b; _ } = Span.compare a b
end

type t = { mutable errors : Error.t list }

let make () = { errors = [] }
let report t tag span message = t.errors <- { tag; span; message; details = None } :: t.errors

let report_detailed t tag span message details =
  t.errors <- { tag; span; message; details = Some details } :: t.errors

let has_problems { errors } = not (CCList.is_empty errors)
let errors { errors } = errors

let error_ansi : level -> Fmt.color = function
  | Critical | Error -> `Red
  | Warning -> `Yellow
  | Note -> `Blue

let display_line out line { Error.tag; span; message; details } =
  let start_l = Span.start_line span
  and start_c = Span.start_col.get span
  and finish_l = Span.finish_line span
  and finish_c = Span.finish_col.get span in
  let line_no = start_l |> string_of_int in

  let pp_pos out () =
    Fmt.fmt "%s:[%d:%d-%d:%d]: %s [%s]@\n" out (Span.filename span).name start_l start_c finish_l
      finish_c message tag.name
  in
  Fmt.styled (`Fg (`Hi (error_ansi tag.level))) pp_pos out ();

  (match details with
  | None -> ()
  | Some details -> Format.fprintf out "%t@\n" details);
  let fmt no line =
    let pp_line out () = Fmt.fmt " %*s" out (String.length line_no) no in
    Fmt.styled (`Fg (`Hi `Green)) pp_line out ();
    if line = "" then Format.fprintf out " │@\n" else Format.fprintf out " │ %s@\n" line
  in
  fmt "" "";
  fmt line_no (CCString.replace ~sub:"\t" ~by:" " line);
  let length =
    if finish_l = start_l then finish_c - start_c + 1 else String.length line - start_c + 1
  in
  fmt "" (String.make (start_c - 1) ' ' ^ String.make length '^')

let summary out t =
  let errors, warnings =
    List.fold_left
      (fun (errors, warnings) { Error.tag; _ } ->
        match tag.level with
        | Critical | Error -> (errors + 1, warnings)
        | Warning | Note -> (errors, warnings + 1))
      (0, 0) t.errors
  in
  if errors > 0 then Format.fprintf out "%d errors and %d warnings@\n" errors warnings
  else if warnings > 0 then Format.fprintf out "No errors and %d warnings@\n" warnings
  else ()

let each_error f store = store.errors |> List.sort Error.span_compare |> List.iter f

let display_of_files ?(out = Format.err_formatter) ?(with_summary = true) store =
  let last = ref None in
  let get_channel name : in_channel =
    match !last with
    | Some (last_name, ch) when name = last_name -> ch
    | Some (_, ch) ->
        close_in ch;
        let ch = open_in name in
        last := Some (name, ch);
        ch
    | None ->
        let ch = open_in name in
        last := Some (name, ch);
        ch
  in
  store
  |> each_error (fun ({ span; _ } as err) ->
         let line =
           match (Span.filename span).path with
           | None -> ""
           | Some path -> (
               let ch = get_channel (Fpath.to_string path) in
               seek_in ch (Span.start_bol span);
               try input_line ch with End_of_file -> "")
         in
         display_line out line err);
  (match !last with
  | Some (_, ch) -> close_in ch
  | _ -> ());
  if with_summary then summary out store

let display_of_string ?(out = Format.err_formatter) ?(with_summary = true) getter store =
  store
  |> each_error (fun ({ span; _ } as err) ->
         match getter (Span.filename span) with
         | None -> ()
         | Some contents ->
             let line =
               let bol = Span.start_bol span in
               String.sub contents bol
                 ((match String.index_from_opt contents bol '\n' with
                  | None -> String.length contents
                  | Some x -> x)
                 - bol)
             in
             display_line out line err);

  if with_summary then summary out store
