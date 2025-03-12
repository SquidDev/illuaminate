type pos = Pos of int [@@unboxed]
type line = int
type column = int

type store =
  | Array of int IArray.t
  | Map of int CCVector.vector * Lexing.lexbuf

type t = { mutable store : store }

module Builder = struct
  type nonrec t = t

  let new_line = function
    | { store = Array _; _ } -> failwith "new_line: Called after finalising"
    | { store = Map (lines, buf); _ } ->
        Lexing.new_line buf;
        let { Lexing.pos_lnum; pos_bol; _ } = buf.lex_curr_p in
        if pos_lnum - 1 <> CCVector.length lines then
          failwith "new_line: non-sequential line numbers";
        CCVector.push lines pos_bol

  let using buf f =
    if not (Lexing.with_positions buf) then
      invalid_arg "using: Lexing.lexbuf should be tracking positions";
    buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = "input" };
    let vec = CCVector.create_with ~capacity:32 0 in
    CCVector.push vec 0;
    let lines = { store = Map (vec, buf) } in

    let res = f lines lines in

    (* Update the backing map to an immutable array. *)
    match lines.store with
    | Array _ -> failwith "using: lines has become an array"
    | Map (xs, _) ->
        new_line lines;
        lines.store <- Array (CCVector.to_array xs |> IArray.of_array);
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
  | { store = Array xs; _ } -> find ~offset (IArray.get xs) 0 (IArray.length xs - 2)
  | { store = Map (xs, _); _ } -> find ~offset (CCVector.get xs) 0 (CCVector.length xs - 2)

let position_of lines (Pos offset) =
  let line, bol = find_line ~offset lines in
  (line + 1, offset - bol + 1)

let position_bol lines (Pos offset) =
  let _, bol = find_line ~offset lines in
  Pos bol

let max_position lines =
  let pos =
    match lines with
    | { store = Array xs; _ } -> IArray.get xs (IArray.length xs - 1)
    | { store = Map _; _ } -> Int.max_int
  in
  Pos pos
