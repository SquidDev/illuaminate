module Filename = struct
  type t =
    { name : string;
      path : Fpath.t option;
      id : string;
      hash : int
    }

  let mk ?path ?name id =
    ( match path with
    | Some p when not (Fpath.is_abs p) ->
        Format.asprintf "Filename.mk: path %a must be absolute" Fpath.pp p |> invalid_arg
    | _ -> () );
    { name = Option.value ~default:id name; path; id; hash = Hashtbl.hash id }

  let compare l r = String.compare l.id r.id

  let hash x = x.hash

  let equal l r = l == r || (l.hash = r.hash && l.id = r.id)

  let pp out f = Format.pp_print_string out f.id
end

type filename = Filename.t =
  { name : string;
    path : Fpath.t option;
    id : string;
    hash : int
  }

module Lines = struct
  module IntMap = Map.Make (Int)

  type store =
    | Array of int array
    | Map of int IntMap.t * Lexing.lexbuf

  type t =
    { file : filename;
      mutable store : store
    }

  let new_line = function
    | { store = Array _; _ } -> failwith "new_line: Called after finalising"
    | { store = Map (lines, buf); _ } as st ->
        Lexing.new_line buf;
        let { Lexing.pos_lnum; pos_bol; _ } = buf.lex_curr_p in
        st.store <- Map (IntMap.add (pos_lnum - 1) pos_bol lines, buf)

  let using file buf f =
    if not (Lexing.with_positions buf) then
      invalid_arg "using: Lexing.lexbuf should be tracking positions";
    buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = file.id };
    let lines = { file; store = Map (IntMap.singleton 0 0, buf) } in

    let res = f lines in

    (* Update the backing map to an array. Should be faster to search than a tree. *)
    match lines.store with
    | Array _ -> failwith "using: lines has become an array"
    | Map (xs, _) ->
        let arr = Array.make (fst (IntMap.max_binding xs) + 1) 0 in
        IntMap.iter (fun line pos -> arr.(line) <- pos) xs;
        lines.store <- Array arr;
        res

  let find ~offset idx =
    let rec go start finish =
      if start = finish then (start, idx start)
      else
        (* If our offset occurs before the start of the current line, search before there. *)
        let mid = (start + finish + 1) / 2 in
        if offset < idx mid then go start (mid - 1) else go mid finish
    in
    fun s f -> go s f

  let find_line ~offset = function
    | _ when offset <= 0 -> (0, 0)
    | { store = Array xs; _ } -> find ~offset (Array.get xs) 0 (Array.length xs - 1)
    | { store = Map (xs, _); _ } ->
        find ~offset (Fun.flip IntMap.find xs) 0 (IntMap.max_binding xs |> fst)

  let get_length line = function
    | { store = Array xs; _ } ->
        let last = Array.length xs - 1 in
        if line >= last then 0 else xs.(line + 1) - xs.(line)
    | { store = Map (xs, _); _ } ->
        let last, _ = IntMap.max_binding xs in
        if line >= last then Int.max_int else IntMap.find (line + 1) xs - IntMap.find line xs

  let get_line lines offset =
    let line, _ = find_line ~offset lines in
    line + 1

  let get_col lines offset =
    let _, bol = find_line ~offset lines in
    offset - bol + 1

  let get_pos lines offset =
    let line, bol = find_line ~offset lines in
    (line + 1, offset - bol + 1)

  let fail_len kind ~line ~col ~max =
    Printf.sprintf "%s invalid for %d:%d: 0 < %s <= %d" kind line col kind max |> invalid_arg

  let over_pos f lines offset =
    let line, bol = find_line ~offset lines in
    let line', col' = f (line + 1, offset - bol + 1) in
    let line_off = line' - 1 in

    if line' < 0 then fail_len "line" ~line:line' ~col:col' ~max:line';
    let length = get_length line_off lines in
    if col' <= 0 || col' > length then fail_len "col" ~line:line' ~col:col' ~max:length;

    match lines with
    | { store = Array xs; _ } -> xs.(line_off) + col' - 1
    | { store = Map (xs, _); _ } -> IntMap.find line_off xs + col' - 1

  let over_col f = over_pos (Lens.Lenses.snd.over f)

  let over_offset f lines offset =
    let offset = f offset in
    let max =
      match lines with
      | { store = Array xs; _ } -> xs.(Array.length xs - 1)
      | { store = Map (xs, _); _ } -> IntMap.max_binding xs |> snd
    in
    if offset < 0 || offset >= max then
      Printf.sprintf "offset %d invalid: 0 <= offset < %d" offset max |> invalid_arg;
    offset
end

type t =
  { lines : Lines.t;
    start : int;
    finish : int
  }

let filename x = x.lines.file

let start_line { lines; start; _ } = Lines.get_line lines start

let start_bol { lines; start; _ } = Lines.find_line ~offset:start lines |> snd

let start_col =
  { Lens.get = (fun { lines; start; _ } -> Lines.get_col lines start);
    over = (fun f ({ lines; start; _ } as l) -> { l with start = Lines.over_col f lines start })
  }

let start_pos =
  { Lens.get = (fun { lines; start; _ } -> Lines.get_pos lines start);
    over = (fun f ({ lines; start; _ } as l) -> { l with start = Lines.over_pos f lines start })
  }

let start_offset =
  { Lens.get = (fun { start; _ } -> start);
    over = (fun f ({ lines; start; _ } as l) -> { l with start = Lines.over_offset f lines start })
  }

let finish_line { lines; finish; _ } = Lines.get_line lines finish

let finish_col =
  { Lens.get = (fun { lines; finish; _ } -> Lines.get_col lines finish);
    over = (fun f ({ lines; finish; _ } as l) -> { l with finish = Lines.over_col f lines finish })
  }

let finish_pos =
  { Lens.get = (fun { lines; finish; _ } -> Lines.get_pos lines finish);
    over = (fun f ({ lines; finish; _ } as l) -> { l with finish = Lines.over_pos f lines finish })
  }

let finish_offset =
  { Lens.get = (fun { finish; _ } -> finish);
    over =
      (fun f ({ lines; finish; _ } as l) -> { l with finish = Lines.over_offset f lines finish })
  }

let pp out span =
  Format.fprintf out "%s[%d:%d-%d:%d]" (filename span).name (start_line span) (start_col.get span)
    (finish_line span) (finish_col.get span)

let show = Format.asprintf "%a" pp

let compare a b =
  if a.lines.file <> b.lines.file then Filename.compare a.lines.file b.lines.file
  else if a.start <> b.start then Int.compare a.start b.start
  else Int.compare a.finish b.finish

let of_pos2 lines (start : Lexing.position) (fin : Lexing.position) =
  { lines;
    start = start.pos_cnum;
    (* The [fin] will /generally/ be one after our actual finish position, thus we decrement it by
       one. However, we need to special-case eof, as that's a 0-width token.*)
    finish = (if fin.pos_cnum <= start.pos_cnum then start.pos_cnum else fin.pos_cnum - 1)
  }

let of_span2 ({ lines; start; _ } as s) ({ finish; _ } as f) =
  if s == f then s else { lines; start; finish }

let finish s = { s with start = s.finish }

type 'a spanned =
  { value : 'a;
    span : t
  }
[@@deriving show]
