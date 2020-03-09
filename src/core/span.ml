module Filename = struct
  type t =
    { name : string;
      path : Fpath.t option;
      id : string;
      hash : int
    }

  let mk ?path ?name id =
    ( match path with
    | Some p when not (Fpath.is_abs p) -> invalid_arg "Filename.mk: path must be absolute"
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

  let find_offset ~line = function
    | { store = Array xs; _ } ->
        let len = Array.length xs in
        if line < 0 then 0 else if line >= len then xs.(len - 1) else xs.(line)
    | { store = Map (xs, _); _ } -> (
        if line < 0 then 0
        else
          match IntMap.find_opt line xs with
          | Some l -> l
          | None -> IntMap.max_binding xs |> snd )

  let get_line lines offset =
    let line, _ = find_line ~offset lines in
    line + 1

  let get_col lines offset =
    let _, bol = find_line ~offset lines in
    offset - bol + 1

  let over_line f lines offset =
    let line, bol = find_line ~offset lines in
    let line = f (line + 1) - 1 in
    find_offset ~line lines + (offset - bol)

  let over_col f lines offset =
    let line, bol = find_line ~offset lines in
    let col = offset - bol + 1 in
    let col' = f col in
    let offset' = bol + col' - 1 in
    let line', bol' = find_line ~offset:offset' lines in
    if line' <> line then
      Printf.sprintf "Column modification changes line, from %d=>%d(%d):%d to %d=>%d(%d):%d." offset
        line bol col offset' line' bol' col'
      |> failwith;
    offset'
end

type t =
  { lines : Lines.t;
    start : int;
    finish : int
  }

let filename x = x.lines.file

let start_line =
  { Lens.get = (fun { lines; start; _ } -> Lines.get_line lines start);
    over = (fun f ({ lines; start; _ } as l) -> { l with start = Lines.over_line f lines start })
  }

let start_col =
  { Lens.get = (fun { lines; start; _ } -> Lines.get_col lines start);
    over = (fun f ({ lines; start; _ } as l) -> { l with start = Lines.over_col f lines start })
  }

let start_bol { lines; start; _ } = Lines.find_line ~offset:start lines |> snd

let finish_line =
  { Lens.get = (fun { lines; finish; _ } -> Lines.get_line lines finish);
    over = (fun f ({ lines; finish; _ } as l) -> { l with finish = Lines.over_line f lines finish })
  }

let finish_col =
  { Lens.get = (fun { lines; finish; _ } -> Lines.get_col lines finish);
    over = (fun f ({ lines; finish; _ } as l) -> { l with finish = Lines.over_col f lines finish })
  }

let pp out span =
  Format.fprintf out "%s[%d:%d-%d:%d]" (filename span).name (start_line.get span)
    (start_col.get span) (finish_line.get span) (finish_col.get span)

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

let of_span2 { lines; start; _ } { finish; _ } = { lines; start; finish }

let finish s = { s with start = s.finish }

type 'a spanned =
  { value : 'a;
    span : t
  }
[@@deriving show]
