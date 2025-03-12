open IlluaminateCore

let parse_string ~name contents =
  let name = Illuaminate.File_id.mk name in
  let lexbuf = Lexing.from_string contents in
  IlluaminateParser.Lexer.lex name lexbuf

let pp_error out contents err =
  Illuaminate.Console_reporter.display_of_string ~out
    (fun _ -> Some contents)
    [ IlluaminateParser.Error.to_error err ]

let pp_token f : IlluaminateParser.Lexer.token -> unit = function
  | Token t -> Token.pp f t
  | Trivial t -> Node.pp_trivial f t

let pp_lex_result ~name out contents =
  match parse_string ~name contents with
  | Error err -> pp_error out contents err
  | Ok parsed ->
      Array.iter (fun x -> pp_token out x.Span.value; Format.pp_print_newline out ()) parsed

let lex ~name contents = Format.asprintf "%a" (pp_lex_result ~name) contents

let lex_list ~name contents =
  match parse_string ~name contents with
  | Error err -> Format.asprintf "%t" (fun out -> pp_error out contents err) |> Result.error
  | Ok tokens -> Array.map (fun x -> x.Span.value) tokens |> Result.ok

let token_eq : IlluaminateParser.Lexer.token Alcotest.testable = Alcotest.testable pp_token ( = )

let tests =
  Omnomnom.Tests.group "The Lexer"
    [ OmnomnomGolden.of_directory lex ~group:"Golden tests" ~directory:"data/lexer"
        ~extension:".lua" ();
      OmnomnomAlcotest.of_alcotest_case
        ( "Linebreaks",
          `Quick,
          fun () ->
            Alcotest.(check (result (array token_eq) string))
              "Parses linebreaks"
              (Ok
                 [| Token (Ident "x");
                    Trivial (Whitespace "\r\n");
                    Token (Ident "y");
                    Trivial (LineComment "--foo");
                    Trivial (Whitespace "\r\n");
                    Token (String "[[x\r\ny]]");
                    Token EoF
                 |])
              (lex_list ~name:"=in" "x\r\ny--foo\r\n[[x\r\ny]]") )
    ]
