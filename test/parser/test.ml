open IlluaminateCore

let regex =
  let open Re in
  let code_block lang =
    seq [ bol; str ("```" ^ lang); char '\n'; group (non_greedy (rep1 any)); str "\n```"; eol ]
  in
  seq [ code_block "lua"; repn (char '\n') 1 (Some 2); opt (code_block "txt"); opt (char '\n') ]
  |> compile

let parse str =
  match IlluaminateParser.program (Span.Filename.mk "=input") (Lexing.from_string str) with
  | Error err ->
      let errs = Error.make () in
      IlluaminateParser.Error.report errs err.span err.value;
      let pp out errs = Error.display_of_string ~out (fun _ -> Some str) errs in
      Format.printf "%a@?" pp errs
  | Ok parsed -> Format.printf "%a@?" Syntax.pp_program parsed
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      Printf.printf "%s\n%s" (Printexc.to_string e) (Printexc.raw_backtrace_to_string bt)

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
          let lua = Re.Group.get group 1 in

          output_substring stdout input pos (start - pos);
          Printf.printf "```lua\n%s\n```\n\n```txt\n" lua;
          parse lua;
          print_string "```\n";

          go stop
  in

  go 0
