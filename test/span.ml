open IlluaminateCore
open Lens
open Span
open Omnomnom.Tests
open OmnomnomQCheck

let gen_position =
  QCheck.(list (int_bound 100))
  |> QCheck.map_keep_input @@ fun offsets ->
     let buf = Lexing.from_string "" in
     Lines.using (Filename.mk "=") buf @@ fun l ->
     List.fold_left
       (fun (line, bol) offset ->
         let bol = bol + offset + 1 in
         buf.lex_curr_p <- { buf.lex_curr_p with pos_cnum = bol; pos_lnum = line };
         Lines.new_line l;
         (line + 1, bol))
       (1, 0) offsets
     |> ignore;

     of_pos2 l
       { pos_bol = 0; pos_lnum = 1; pos_cnum = 0; pos_fname = "" }
       { pos_bol = 0; pos_lnum = 1; pos_cnum = 1; pos_fname = "" }

let tests =
  let incr_self lens (xs, pos) =
    QCheck.assume (xs <> [] && List.hd xs <> 0);
    (pos |> lens %= ( + ) 1) ^. lens = (pos ^. lens) + 1
  and incr_other lens other (xs, pos) =
    QCheck.assume (xs <> [] && List.hd xs <> 0);
    (pos |> lens %= ( + ) 1) ^. other = pos ^. other
  in
  let mk ~name ?count = QCheck.Test.make ~name ?count gen_position in
  [ mk ~count:1000 ~name:"Can increment start line" (incr_self start_line);
    mk ~count:1000 ~name:"Can increment start column" (incr_self start_col);
    mk ~count:1000 ~name:"Can increment finish line" (incr_self finish_line);
    mk ~count:1000 ~name:"Can increment finish column" (incr_self finish_col);
    mk ~count:1000 ~name:"Changing line preserves column" (incr_other start_line start_col);
    mk ~count:1000 ~name:"Changing column preserves line" (incr_other start_col start_line)
  ]
  |> List.map of_qcheck |> group "Spans"
