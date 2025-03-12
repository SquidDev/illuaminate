module Span = IlluaminateCore.Span
module I = Grammar.MenhirInterpreter
module PE = Lrgrep_runtime.Interpreter (Parse_errors.Table_error_message) (I)
module Error = Error

type located_token = Token.lexer_token * Lexing.position * Lexing.position

let lex_one lines (lexbuf : Lexing.lexbuf) : located_token =
  let start = lexbuf.lex_curr_p in
  let token = Lexer.token lines lexbuf in
  let finish = lexbuf.lex_curr_p in
  (token, start, finish)

let rec lex_leading_worker lines (lexbuf : Lexing.lexbuf) xs =
  let start = lexbuf.lex_curr_p in
  match Lexer.token lines lexbuf with
  | TRIVIA value ->
      lex_leading_worker lines lexbuf
        ({ Span.value; span = Span.of_pos2 lines start lexbuf.lex_curr_p } :: xs)
  | token -> (List.rev xs, (token, start, lexbuf.lex_curr_p))

let lex_leading lines lexbuf (token : located_token) =
  match token with
  | TRIVIA value, start, finish ->
      lex_leading_worker lines lexbuf [ { Span.value; span = Span.of_pos2 lines start finish } ]
  | tok -> ([], tok)

let lex_trailing lines (lexbuf : Lexing.lexbuf) prev_line =
  let rec go xs =
    let start = lexbuf.lex_curr_p in
    match Lexer.token lines lexbuf with
    | TRIVIA value when start.pos_lnum = prev_line ->
        go ({ Span.value; span = Span.of_pos2 lines start lexbuf.lex_curr_p } :: xs)
    | t -> (List.rev xs, (t, start, lexbuf.lex_curr_p))
  in
  go []

let get_error_message token ~pre_env ~post_env : Error.message =
  match
    PE.run pre_env
    |> List.find_map (fun x -> Parse_errors.execute_error_message x Lexing.dummy_pos token)
  with
  | Some x -> x
  | None ->
      let state =
        match I.top post_env with
        | None -> 0 (* Should never happen, but... *)
        | Some (I.Element (s, _, _, _)) -> I.number s
      in
      let message = try Messages.message state |> String.trim with Not_found -> "Unknown error" in
      Unexpected_token { token; message }

let parse start (file : Illuaminate.File_id.t) (lexbuf : Lexing.lexbuf) =
  Span.Lines.using file lexbuf @@ fun lines ->
  let position_map = Span.Lines.position_map lines in
  let rec go env token next = function
    | I.InputNeeded env as checkpoint -> go_input env checkpoint next
    | (I.Shifting _ | I.AboutToReduce _) as checkpoint -> I.resume checkpoint |> go env token next
    | I.HandlingError post_env ->
        let message = get_error_message token ~pre_env:env ~post_env in
        Error { Error.file; position_map; message }
    | I.Accepted x -> Ok x
    | I.Rejected -> assert false
  and go_input env checkpoint token =
    let leading_trivia, ((token, start, finish) as lex_token) = lex_leading lines lexbuf token in
    let span = Span.of_pos2 lines start lexbuf.lex_curr_p in
    let token, next =
      match token with
      | EOF ->
          (* Just return the current "next" token (we won't inspect it after all, and an EOF token
             with no trailing data. *)
          ( (Token.make_token ~leading_trivia ~trailing_trivia:[] ~span token, start, finish),
            lex_token )
      | _ ->
          let trailing_trivia, next = lex_trailing lines lexbuf start.pos_lnum in
          ((Token.make_token ~leading_trivia ~trailing_trivia ~span token, start, finish), next)
    in
    I.offer checkpoint token |> go env token next
  in
  try
    match start Lexing.dummy_pos with
    | I.InputNeeded env as checkpoint -> go_input env checkpoint (lex_one lines lexbuf)
    | _ -> assert false
  with Lexer.Error message -> Error { file; position_map; message }

let program = parse Grammar.Incremental.program
let repl_exprs = parse Grammar.Incremental.repl_exprs

module Lexer = struct
  type token =
    | Token of string
    | Trivial of IlluaminateCore.Node.trivial

  let lex (file : Illuaminate.File_id.t) (lexbuf : Lexing.lexbuf) =
    Span.Lines.using file lexbuf @@ fun lines ->
    try
      let rec go xs =
        let token, start, finish = lex_one lines lexbuf in
        let span = Span.of_pos2 lines start finish in
        let value =
          match token with
          | TRIVIA t -> Trivial t
          | t ->
              Token
                (Token.make_token ~leading_trivia:[] ~trailing_trivia:[] ~span t |> Token.to_string)
        in
        let xs = { Span.value; span } :: xs in
        match token with
        | EOF -> xs
        | _ -> go xs
      in
      go [] |> List.rev |> Array.of_list |> Result.ok
    with Lexer.Error message ->
      Error { Error.file; position_map = Span.Lines.position_map lines; message }
end
