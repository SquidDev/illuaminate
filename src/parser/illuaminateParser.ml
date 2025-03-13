module Span = IlluaminateCore.Span
module I = Grammar.MenhirInterpreter
module PE = Lrgrep_runtime.Interpreter (Parse_errors.Table_error_message) (I)
module Error = Error

type located_token = Token.lexer_token * Lexing.position * Lexing.position
type trivia_buf = IlluaminateCore.Node.Trivia.t CCVector.vector

let take_vector xs =
  let contents = CCVector.to_array xs |> Illuaminate.IArray.of_array in
  CCVector.clear xs; contents

let lex_one lines (lexbuf : Lexing.lexbuf) : located_token =
  let start = lexbuf.lex_curr_p in
  let token = Lexer.token lines lexbuf in
  let finish = lexbuf.lex_curr_p in
  (token, start, finish)

let rec lex_leading_worker lines (lexbuf : Lexing.lexbuf) (trivia : trivia_buf) =
  let start = lexbuf.lex_curr_p in
  match Lexer.token lines lexbuf with
  | TRIVIA (kind, contents) ->
      CCVector.push trivia { kind; contents; start = Pos start.pos_cnum };
      lex_leading_worker lines lexbuf trivia
  | token -> (token, start, lexbuf.lex_curr_p)

let lex_leading lines lexbuf (trivia : trivia_buf) (token : located_token) =
  match token with
  | TRIVIA (kind, contents), start, _ ->
      CCVector.push trivia { kind; contents; start = Pos start.pos_cnum };
      let t = lex_leading_worker lines lexbuf trivia in
      (take_vector trivia, t)
  | tok -> (Illuaminate.IArray.empty, tok)

let rec lex_trailing_worker lines (lexbuf : Lexing.lexbuf) (trivia : trivia_buf) prev_line =
  let start = lexbuf.lex_curr_p in
  match Lexer.token lines lexbuf with
  | TRIVIA (kind, contents) when start.pos_lnum = prev_line ->
      CCVector.push trivia { kind; contents; start = Pos start.pos_cnum };
      lex_trailing_worker lines lexbuf trivia prev_line
  | token -> (token, start, lexbuf.lex_curr_p)

let lex_trailing lines (lexbuf : Lexing.lexbuf) trivia prev_line =
  let t = lex_trailing_worker lines lexbuf trivia prev_line in
  (take_vector trivia, t)

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
  let trivia = CCVector.create () in
  let rec go env token next = function
    | I.InputNeeded env as checkpoint -> go_input env checkpoint next
    | (I.Shifting _ | I.AboutToReduce _) as checkpoint -> I.resume checkpoint |> go env token next
    | I.HandlingError post_env ->
        let message = get_error_message token ~pre_env:env ~post_env in
        Error { Error.file; position_map; message }
    | I.Accepted x -> Ok x
    | I.Rejected -> assert false
  and go_input env checkpoint token =
    let leading_trivia, ((token, start, finish) as lex_token) =
      lex_leading lines lexbuf trivia token
    in
    let span = Span.of_pos2 lines start lexbuf.lex_curr_p in
    let token, next =
      match token with
      | EOF ->
          (* Just return the current "next" token (we won't inspect it after all, and an EOF token
             with no trailing data. *)
          ( ( Token.make_token ~leading_trivia ~trailing_trivia:Illuaminate.IArray.empty ~span token,
              start,
              finish ),
            lex_token )
      | _ ->
          let trailing_trivia, next = lex_trailing lines lexbuf trivia start.pos_lnum in
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
    | Trivial of IlluaminateCore.Node.Trivia.kind * string

  let lex (file : Illuaminate.File_id.t) (lexbuf : Lexing.lexbuf) =
    Span.Lines.using file lexbuf @@ fun lines ->
    try
      let rec go xs =
        let token, start, finish = lex_one lines lexbuf in
        let span = Span.of_pos2 lines start finish in
        let value =
          match token with
          | TRIVIA (k, c) -> Trivial (k, c)
          | t ->
              Token
                (Token.make_token ~leading_trivia:Illuaminate.IArray.empty
                   ~trailing_trivia:Illuaminate.IArray.empty ~span t
                |> Token.to_string)
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
