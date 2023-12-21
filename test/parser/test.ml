open IlluaminateCore

let parse out parser pp str =
  match parser (Illuaminate.File_id.mk "=input") (Lexing.from_string str) with
  | Error ({ span; value = err } : IlluaminateParser.Error.t Span.spanned) -> (
      let errs = Error.make () in
      IlluaminateParser.Error.report errs span err;
      Error.display_of_string ~out ~with_summary:false (fun _ -> Some str) errs;
      match err with
      | IlluaminateParser.Error.Unexpected_token _ -> Format.fprintf out "(from messages.txt)\n"
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
