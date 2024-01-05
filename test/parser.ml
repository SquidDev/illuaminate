open IlluaminateCore

let parse ~name contents =
  let lexbuf = Lexing.from_string contents in
  let name = Illuaminate.File_id.mk name in
  match IlluaminateParser.program name lexbuf with
  | Error err ->
      Format.asprintf "%t" @@ fun out ->
      Illuaminate.Console_reporter.display_of_string ~out ~with_summary:false
        (fun _ -> Some contents)
        [ IlluaminateParser.Error.to_error err ]
  | Ok parsed -> Syntax.show_program parsed

let tests =
  OmnomnomGolden.of_directory parse ~group:"The parser" ~directory:"data/parser" ~extension:".lua"
    ()
