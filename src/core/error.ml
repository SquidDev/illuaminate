module StringMap = Map.Make (String)

module Style = struct
  type ansi_color =
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White

  type t =
    | Unstyled
    | DullColor of ansi_color
    | BrightColor of ansi_color
    | Underlined
    | Styled of t list

  type Format.stag += Style of t

  let printf style out fmt =
    Format.pp_open_stag out (Style style);
    Format.kfprintf (fun x -> Format.pp_close_stag x ()) out fmt

  let setup_ansi out =
    let prev = Format.pp_get_formatter_stag_functions out () in
    let stack = ref [ "\027[0m" ] in
    let get_color = function
      | Black -> 0
      | Red -> 1
      | Green -> 2
      | Yellow -> 3
      | Blue -> 4
      | Magenta -> 5
      | Cyan -> 6
      | White -> 7
    in
    let rec get_style = function
      | Unstyled -> "\027[0m"
      | Underlined -> "\027[0m"
      | DullColor c -> Printf.sprintf "\027[3%dm" (get_color c)
      | BrightColor c -> Printf.sprintf "\027[1;3%dm" (get_color c)
      | Styled c -> List.map get_style c |> String.concat ""
    in
    Format.pp_set_mark_tags out true;
    Format.pp_set_margin out 120;
    Format.pp_set_formatter_stag_functions out
      { prev with
        mark_open_stag =
          (function
          | Style s ->
              let style = get_style s in
              stack := style :: !stack;
              style
          | x -> prev.mark_open_stag x);
        mark_close_stag =
          (function
          | Style _ ->
              let style =
                match !stack with
                | _ :: (x :: _ as xs) ->
                    stack := xs;
                    x
                | _ -> failwith "Popping from an empty stack"
              in
              "\027[0m" ^ style
          | x -> prev.mark_close_stag x)
      }
end

type level =
  | Critical
  | Error
  | Warning
  | Note

module Tag = struct
  type t =
    { name : string;
      level : level
    }

  let pp f { name; _ } = Format.fprintf f "%s" name

  let tags = ref StringMap.empty

  let make level name =
    let tag = { level; name } in
    tags := StringMap.add name tag !tags;
    tag

  let find name = StringMap.find_opt name !tags

  let compare l r = String.compare l.name r.name

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

let error_ansi = function
  | Critical | Error -> Style.Red
  | Warning -> Style.Yellow
  | Note -> Style.Blue

let display_line out line { Error.tag; span; message; details } =
  let line_no = string_of_int span.start_line in
  Style.(printf (BrightColor (error_ansi tag.level)))
    out "%s:[%d:%d-%d:%d]: %s [%s]@\n" span.filename.name span.start_line span.start_col
    span.finish_line span.finish_col message tag.name;
  ( match details with
  | None -> ()
  | Some details -> Format.fprintf out "%t@\n" details );
  let fmt no line =
    Style.(printf (BrightColor Green)) out " %*s" (String.length line_no) no;
    if line = "" then Format.fprintf out " │@\n" else Format.fprintf out " │ %s@\n" line
  in
  fmt "" "";
  fmt line_no (CCString.replace ~sub:"\t" ~by:" " line);
  let length =
    if span.finish_line = span.start_line then span.finish_col - span.start_col
    else String.length line - span.start_col + 1
  in
  fmt "" (String.make (span.start_col - 1) ' ' ^ String.make length '^')

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

let display_of_files ?(out = Format.err_formatter) store =
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
         let ch = get_channel span.filename.path in
         seek_in ch span.start_bol;
         let line = input_line ch in
         display_line out line err);
  ( match !last with
  | Some (_, ch) -> close_in ch
  | _ -> () );
  summary out store

let display_of_string ?(out = Format.err_formatter) getter store =
  store
  |> each_error (fun ({ span; _ } as err) ->
         match getter span.filename with
         | None -> ()
         | Some contents ->
             let line =
               String.sub contents span.start_bol
                 ( ( match String.index_from_opt contents span.start_bol '\n' with
                   | None -> String.length contents
                   | Some x -> x )
                 - span.start_bol )
             in
             display_line out line err);
  summary out store
