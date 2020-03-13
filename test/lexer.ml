open IlluaminateCore

let lex ~name contents =
  let lexbuf = Lexing.from_string contents in
  let errs = Error.make () in
  let name = Span.Filename.mk name in
  let buffer = Buffer.create 128 in
  let out = Format.formatter_of_buffer buffer in
  ( match IlluaminateParser.Lexer.lex name lexbuf with
  | Error err ->
      IlluaminateParser.Error.report errs err.span err.value;
      Error.display_of_string ~out (fun _ -> Some contents) errs
  | Ok parsed ->
      let pp_token f : IlluaminateParser.Lexer.token -> unit = function
        | Token t -> Token.pp f t
        | Trivial t -> Node.pp_trivial f t
      in
      Array.iter (fun x -> pp_token out x.Span.value; Format.pp_print_newline out ()) parsed );
  Format.pp_print_flush out (); Buffer.contents buffer

let tests =
  OmnomnomGolden.of_directory lex ~group:"The lexer" ~directory:"data/lexer" ~extension:".lua" ()
