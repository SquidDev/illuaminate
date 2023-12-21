module IntMap = Map.Make (Int)

type pos = Pos of int [@@unboxed]
type line = int
type column = int

type store =
  | Array of int array
  | Map of int IntMap.t * Lexing.lexbuf

type t = { mutable store : store }

module Builder = struct
  type nonrec t = t

  let new_line = function
    | { store = Array _; _ } -> failwith "new_line: Called after finalising"
    | { store = Map (lines, buf); _ } as st ->
        Lexing.new_line buf;
        let { Lexing.pos_lnum; pos_bol; _ } = buf.lex_curr_p in
        st.store <- Map (IntMap.add (pos_lnum - 1) pos_bol lines, buf)

  let using buf f =
    if not (Lexing.with_positions buf) then
      invalid_arg "using: Lexing.lexbuf should be tracking positions";
    buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = "input" };
    let lines = { store = Map (IntMap.singleton 0 0, buf) } in

    let res = f lines lines in

    (* Update the backing map to an array. Should be faster to search than a tree. *)
    match lines.store with
    | Array _ -> failwith "using: lines has become an array"
    | Map (xs, _) ->
        let current = buf.lex_curr_p in
        let xs = IntMap.add current.pos_lnum current.pos_cnum xs in

        let arr = Array.make (fst (IntMap.max_binding xs) + 1) 0 in
        IntMap.iter (fun line pos -> arr.(line) <- pos) xs;
        lines.store <- Array arr;
        (res, lines)
end

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
  | { store = Array xs; _ } -> find ~offset (Array.get xs) 0 (Array.length xs - 2)
  | { store = Map (xs, _); _ } ->
      find ~offset (Fun.flip IntMap.find xs) 0 (IntMap.max_binding xs |> fst)

let position_of lines (Pos offset) =
  let line, bol = find_line ~offset lines in
  (line + 1, offset - bol + 1)

let position_bol lines (Pos offset) =
  let _, bol = find_line ~offset lines in
  Pos bol

let max_position lines =
  let pos =
    match lines with
    | { store = Array xs; _ } -> xs.(Array.length xs - 1)
    | { store = Map _; _ } -> Int.max_int
  in
  Pos pos
