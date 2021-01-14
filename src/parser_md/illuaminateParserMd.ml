open IlluaminateCore

let spanned ~lines ~(lexbuf : Lexing.lexbuf) ~start value =
  { Span.span = Span.of_pos2 lines start lexbuf.lex_curr_p; value }

let rec parse_attributes lines (lexbuf : Lexing.lexbuf) acc =
  let start = lexbuf.lex_curr_p in
  match Lexer.header lines lexbuf with
  | End | Malformed -> List.rev acc
  | Blank -> parse_attributes lines lexbuf acc
  | Key key ->
      let key = spanned ~lines ~lexbuf ~start key in

      let start = lexbuf.lex_curr_p in
      let value = Lexer.header_contents lines lexbuf |> spanned ~lines ~lexbuf ~start in
      parse_attributes lines lexbuf ((key, value) :: acc)

let parse file lexbuf =
  Span.Lines.using file lexbuf @@ fun lines ->
  let attributes = if Lexer.start lines lexbuf then parse_attributes lines lexbuf [] else [] in
  let contents =
    let start = lexbuf.lex_curr_p in
    Lexer.body (Buffer.create 32) lines lexbuf |> spanned ~lines ~lexbuf ~start
  in
  (attributes, contents)
