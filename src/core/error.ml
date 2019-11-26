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
end

module TagSet = Set.Make (Tag)

type store = { mutable errors : (bool * Tag.t * Span.t * string) list }

type t =
  | Root of store
  | Mute of
      { muted : TagSet.t;
        wraps : store
      }

let unwrap = function
  | Root store -> store
  | Mute { wraps; _ } -> wraps

let muted tag = function
  | Root _ -> false
  | Mute { muted; _ } -> TagSet.mem tag muted

let make () = Root { errors = [] }

let mute errs base =
  let store, extra_muted =
    match base with
    | Root store -> (store, TagSet.empty)
    | Mute { muted; wraps } -> (wraps, muted)
  in
  Mute { wraps = store; muted = TagSet.union (TagSet.of_list errs) extra_muted }

let compare (_, _, (a : Span.t), _) (_, _, (b : Span.t), _) =
  if a.filename <> b.filename then String.compare a.filename.name b.filename.name
  else if a.start_line <> b.start_line then compare a.start_line b.start_line
  else compare a.start_col b.start_col

let report t tag span message =
  let store = unwrap t in
  store.errors <- (muted tag t, tag, span, message) :: store.errors

let error_ansi = function
  | Critical | Error -> Style.Red
  | Warning -> Style.Yellow

let display_line out line (tag : Tag.t) (pos : Span.t) message =
  let line_no = string_of_int pos.start_line in
  Style.(printf (BrightColor (error_ansi tag.level)))
    out "%s:[%d:%d-%d:%d]: %s [%s]@\n" pos.filename.name pos.start_line pos.start_col
    pos.finish_line pos.finish_col message tag.name;
  let fmt no line =
    Style.(printf (BrightColor Green)) out "%*s " (String.length line_no) no;
    if line = "" then Format.fprintf out " │@\n" else Format.fprintf out " │ %s@\n" line
  in
  fmt "" "";
  fmt line_no (CCString.replace ~sub:"\t" ~by:" " line);
  let length =
    if pos.finish_line = pos.start_line then pos.finish_col - pos.start_col
    else String.length line - pos.start_col
  in
  fmt "" (String.make pos.start_col ' ' ^ String.make length '^')

let summary out t =
  let errors, warnings =
    List.fold_left
      (fun (errors, warnings) (_, (tag : Tag.t), _, _) ->
        match tag.level with
        | Critical | Error -> (errors + 1, warnings)
        | Warning -> (errors, warnings + 1))
      (0, 0) (unwrap t).errors
  in
  if errors > 0 then
    Format.fprintf out "Compilation failed with %d errors and %d warnings@\n" errors warnings
  else if warnings > 0 then Format.fprintf out "Compilation succeeded with %d warnings@\n" warnings
  else Format.fprintf out "Compilation succeeded with no warnings@\n"

let each_error f t =
  t.errors
  |> List.filter (fun (muted, _, _, _) -> not muted)
  |> List.sort compare
  |> List.iter (fun (_, tag, (pos : Span.t), message) -> f tag pos message)

let display_of_channel ?(out = Format.err_formatter) getter t =
  let last = ref None in
  let get_channel name =
    match !last with
    | Some (last_name, ch) when name = last_name -> ch
    | Some (_, ch) ->
        ( match ch with
        | Some ch -> close_in ch
        | _ -> () );
        let ch = getter name in
        last := Some (name, ch);
        ch
    | None ->
        let ch = getter name in
        last := Some (name, ch);
        ch
  in
  unwrap t
  |> each_error (fun tag (pos : Span.t) message ->
         match get_channel pos.filename with
         | None -> ()
         | Some ch ->
             seek_in ch pos.start_bol;
             let line = input_line ch in
             display_line out line tag pos message);
  ( match !last with
  | Some (_, Some ch) -> close_in ch
  | _ -> () );
  summary out t

let display_of_string ?(out = Format.err_formatter) getter t =
  unwrap t
  |> each_error (fun tag (pos : Span.t) message ->
         match getter pos.filename with
         | None -> ()
         | Some contents ->
             let line =
               String.sub contents pos.start_bol
                 ( ( match String.index_from_opt contents pos.start_bol '\n' with
                   | None -> String.length contents
                   | Some x -> x )
                 - pos.start_bol )
             in
             display_line out line tag pos message);
  summary out t
