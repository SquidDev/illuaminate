open IlluaminateCore
open Lens
open Span
open Omnomnom.Tests
open OmnomnomQCheck

let build lines ~start ~finish =
  let buf = Lexing.from_string "" in
  Lines.using (Filename.mk "=") buf @@ fun l ->
  Array.fold_left
    (fun (line, bol) offset ->
      let bol = bol + offset + 1 in
      buf.lex_curr_p <- { buf.lex_curr_p with pos_cnum = bol; pos_lnum = line };
      Lines.new_line l;
      (line + 1, bol))
    (1, 0) lines
  |> ignore;

  of_pos2 l start finish

let build' lines ~line ~col =
  build lines
    ~start:{ pos_bol = 0; pos_lnum = 1; pos_cnum = 1; pos_fname = "" }
    ~finish:{ pos_bol = 0; pos_lnum = 1; pos_cnum = 2; pos_fname = "" }
  |> start_pos ^= (line, col)
  |> finish_pos ^= (line, col)

let gen_position =
  QCheck.(array_of_size (Gen.pure 2) (int_bound 100))
  |> QCheck.map_keep_input @@ fun lines ->
     build lines
       ~start:{ pos_bol = 0; pos_lnum = 1; pos_cnum = 0; pos_fname = "" }
       ~finish:{ pos_bol = 0; pos_lnum = 1; pos_cnum = 1; pos_fname = "" }

let quickcheck =
  let incr_self lens (xs, pos) =
    QCheck.assume (xs.(0) <> 0);
    (pos |> lens %= ( + ) 1) ^. lens = (pos ^. lens) + 1
  and incr_other lens other (xs, pos) =
    QCheck.assume (xs.(0) <> 0);
    (pos |> lens %= ( + ) 1) ^. other = pos ^. other
  in
  let mk ~name ?count = QCheck.Test.make ~name ?count gen_position in
  [ mk ~count:1000 ~name:"Can increment start line" (incr_self (start_pos -| Lenses.fst));
    mk ~count:1000 ~name:"Can increment start column" (incr_self start_col);
    mk ~count:1000 ~name:"Can increment finish line" (incr_self (finish_pos -| Lenses.fst));
    mk ~count:1000 ~name:"Can increment finish column" (incr_self finish_col);
    mk ~count:1000 ~name:"Changing line preserves column"
      (incr_other (start_pos -| Lenses.fst) start_col);
    mk ~count:1000 ~name:"Changing column preserves line"
      (incr_other start_col (start_pos -| Lenses.fst));
    (* A couple of tests to verify that position bounds checks work. *)
    QCheck2.Test.make ~count:1000 ~name:"Can change column within bounds"
      QCheck2.Gen.(pair small_int small_signed_int)
      (fun (len, col) ->
        let in_bounds = col > 0 && col <= len + 1
        and ok =
          match build' [| len |] ~line:1 ~col with
          | _ -> true
          | exception Invalid_argument _ -> false
        in
        in_bounds = ok);
    QCheck2.Test.make ~count:1000 ~name:"Can change line within bounds"
      QCheck2.Gen.(pair small_int small_signed_int)
      (fun (len, line) ->
        let len = len + 1 in
        let in_bounds = line > 0 && line <= len
        and ok =
          match build' (Array.make len 1) ~line ~col:1 with
          | _ -> true
          | exception Invalid_argument _ -> false
        in
        in_bounds = ok)
  ]
  |> List.map of_qcheck |> group "Span"

let tests = group "Span" [ quickcheck ]
