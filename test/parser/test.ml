open IlluaminateCore

let parse out parser pp str =
  let file = Illuaminate.File_id.mk "=input" in
  match parser file (Lexing.from_string str) with
  | Error err -> (
      Illuaminate.Console_reporter.display_of_string ~out ~with_summary:false
        (fun _ -> Some str)
        [ IlluaminateParser.Error.to_error err ];
      match err.message with
      | Unexpected_token _ -> Format.fprintf out "(from messages.txt)\n"
      | _ -> ())
  | Ok parsed -> pp out parsed
  | exception e ->
      let bt = Printexc.get_raw_backtrace () in
      Format.fprintf out "%s\n%s" (Printexc.to_string e) (Printexc.raw_backtrace_to_string bt)

let () =
  Md_test_runner.run @@ fun out kind str ->
  match kind with
  | "" -> parse out IlluaminateParser.program Syntax.pp_program str
  | "{repl_exprs}" -> parse out IlluaminateParser.repl_exprs Syntax.pp_repl_exprs str
  | _ -> failwith "Invalid parser"
