open IlluaminateCore

let parse ~name contents =
  let lexbuf = Lexing.from_string contents in
  let errs = Error.make () in
  let name = { Span.name; path = name } in
  match IlluaminateParser.parse name lexbuf with
  | Error err ->
      let buffer = Buffer.create 128 in
      IlluaminateParser.Error.report errs err.span err.value;
      Error.display_of_string ~out:(Format.formatter_of_buffer buffer) (fun _ -> Some contents) errs;
      Buffer.contents buffer
  | Ok parsed -> Syntax.show_program parsed

let tests =
  OmnomnomGolden.of_directory parse ~group:"The parser" ~directory:"data/parser" ~extension:".lua"
    ()
