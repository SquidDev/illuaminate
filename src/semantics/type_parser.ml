open Type_grammar

let show_token = function
  | TRUE -> "true"
  | NIL -> "nil"
  | FUNCTION -> "function"
  | FALSE -> "false"
  | COMMA -> ","
  | COLON -> ":"
  | DOT -> "."
  | DOTS -> "..."
  | EQUALS -> "="
  | SEMICOLON -> ";"
  | PIPE -> "|"
  | ASKING -> "?"
  | OPAREN -> "("
  | CPAREN -> ")"
  | OBRACE -> "{"
  | CBRACE -> "}"
  | OSQUARE -> "["
  | CSQUARE -> "]"
  | STRING _ -> "string"
  | INT _ | NUMBER _ -> "number"
  | IDENT _ -> "ident"
  | EOF -> "eof"

let show_error = function
  | Type_lexer.UnexpectedCharacter c ->
      Printf.sprintf "Unexpected character \"%s\"" (String.escaped c)
  | UnterminatedString -> "Unterminated string"
  | UnexpectedToken (tok, m) -> Printf.sprintf "Unexpected `%s`: %s" (show_token tok) m
  | SyntaxError m -> m

let parse_from f input =
  let module I = MenhirInterpreter in
  let lexbuf = Lexing.from_string input in
  let supplier = I.lexer_lexbuf_to_supplier Type_lexer.token lexbuf in
  let ok x = Result.Ok x in
  let fail (check : 'a I.checkpoint) =
    match check with
    | I.HandlingError env ->
        let state =
          match Lazy.force (I.stack env) with
          | Nil -> 0 (* Should never happen, but... *)
          | Cons (I.Element (s, _, _, _), _) -> I.number s
        in
        let error =
          try Type_messages.message state |> String.trim with Not_found -> "Unknown error"
        in
        Result.Error error
    | _ -> assert false
  in
  try I.loop_handle ok fail supplier (f lexbuf.lex_curr_p)
  with Type_lexer.Error (e, _, _) -> Result.Error (show_error e)

let parse = parse_from Incremental.main
let parse_vararg = parse_from Incremental.main_vararg
