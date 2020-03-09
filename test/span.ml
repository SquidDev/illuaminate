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
  let change lens (xs, pos) =
    QCheck.assume (xs <> [] && List.hd xs <> 0);
    (pos |> lens %= ( + ) 1) ^. lens = (pos ^. lens) + 1
  in
  [ QCheck.Test.make ~count:1000 ~name:"Modifying start line" gen_position (change start_line);
    QCheck.Test.make ~count:1000 ~name:"Modifying start column" gen_position (change start_col);
    QCheck.Test.make ~count:1000 ~name:"Modifying finish line" gen_position (change finish_line);
    QCheck.Test.make ~count:1000 ~name:"Modifying finish column" gen_position (change finish_col)
  ]
  |> List.map of_qcheck |> group "Spans"
