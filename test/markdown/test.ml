let header = "```````````````````````````````` example\n"
let footer = "\n````````````````````````````````"

let regex =
  let open Re in
  seq
    [ bol;
      str header;
      group (non_greedy (rep1 any));
      str "\n.\n";
      non_greedy (rep1 any);
      str footer;
      eol
    ]
  |> compile

let parse str =
  Cmarkit.Doc.of_string ~strict:false str
  |> Cmarkit_html.of_doc ~safe:false |> String.trim |> print_string

let () =
  Printexc.record_backtrace true;
  let input = In_channel.input_all stdin in

  let limit = String.length input in
  let rec go pos =
    if pos > limit then ()
    else
      match Re.exec_opt ~pos regex input with
      | None -> output_substring stdout input pos (limit - pos)
      | Some group ->
          let start, stop = Re.Group.offset group 0 in
          let md = Re.Group.get group 1 in

          output_substring stdout input pos (start - pos);
          print_string header;
          print_endline md;
          print_endline ".";
          parse md;
          print_string footer;

          go stop
  in

  go 0
