module Span = IlluaminateCore.Span
open Token
open Error
module I = Grammar.MenhirInterpreter
module Error = Error

let lex_one file (lexbuf : Lexing.lexbuf) =
  let start = lexbuf.lex_curr_p in
  let token = Lexer.token lexbuf in
  { Span.value = token; span = Span.of_pos2 file start lexbuf.lex_curr_p }

let lex_leading file lexbuf =
  let rec go xs =
    match lex_one file lexbuf with
    | { Span.value = Trivial value; span } -> go ({ Span.value; span } :: xs)
    | { Span.value = Token value; span } -> (List.rev xs, { Span.value; span })
  in
  go []

let lex_trailing file lexbuf tok_span =
  let rec go xs =
    match lex_one file lexbuf with
    | { Span.value = Trivial value; span = { start_line; _ } as span }
      when start_line = tok_span.Span.start_line ->
        go ({ Span.value; span } :: xs)
    | t -> (List.rev xs, t)
  in
  go []

let lex_token file lexbuf (next : lexer_token Span.spanned) =
  let leading, { Span.value = token; span = tok_span } =
    match next with
    | { Span.value = Trivial value; span } ->
        let leading, t = lex_leading file lexbuf in
        ({ Span.value; span } :: leading, t)
    | { Span.value = Token value; span } -> ([], { Span.value; span })
  in
  match token with
  | EoF ->
      (* Just return the current "next" token (we won't inspect it after all, and an EOF token with
         no trailing data. *)
      (Token.make_token leading [] tok_span token, next)
  | _ ->
      let trailing, next = lex_trailing file lexbuf tok_span in
      (Token.make_token leading trailing tok_span token, next)

let parse start (file : Span.filename) (lexbuf : Lexing.lexbuf) =
  let rec go last next checkpoint =
    match checkpoint with
    | I.InputNeeded _ ->
        let tok, next = lex_token file lexbuf next in
        I.offer checkpoint (tok, Lexing.dummy_pos, Lexing.dummy_pos) |> go (Some tok) next
    | I.Shifting _ | I.AboutToReduce _ -> go last next (I.resume checkpoint)
    | I.HandlingError env -> (
        let state =
          match I.top env with
          | None -> 0 (* Should never happen, but... *)
          | Some (I.Element (s, _, _, _)) -> I.number s
        in
        let error = try Messages.message state |> String.trim with Not_found -> "Unknown error" in
        match last with
        | Some t -> Error { Span.span = Token.get_span t; value = UnexpectedToken (t, error) }
        | None -> assert false )
    | I.Accepted x -> Ok x
    | I.Rejected -> assert false
  in
  try
    lexbuf.lex_curr_p <- { Lexing.pos_fname = file.name; pos_lnum = 1; pos_cnum = 0; pos_bol = 0 };
    let first = lex_one file lexbuf in
    start Lexing.dummy_pos |> go None first
  with Lexer.Error (err, start, fin) ->
    Error { Span.span = Span.of_pos2 file start fin; value = err }

let program = parse Grammar.Incremental.program

let repl_exprs = parse Grammar.Incremental.repl_exprs

module Lexer = struct
  type token = lexer_token =
    | Token of IlluaminateCore.Token.t
    | Trivial of IlluaminateCore.Node.trivial

  let lex (file : Span.filename) (lexbuf : Lexing.lexbuf) :
      (token Span.spanned array, Error.t Span.spanned) result =
    try
      lexbuf.lex_curr_p <- { Lexing.pos_fname = file.name; pos_lnum = 1; pos_cnum = 0; pos_bol = 0 };
      let rec go xs =
        let token = lex_one file lexbuf in
        match token with
        | { value = Token EoF; _ } -> token :: xs
        | _ -> go (token :: xs)
      in
      go [] |> List.rev |> Array.of_list |> Result.ok
    with Lexer.Error (err, start, fin) ->
      Error { Span.span = Span.of_pos2 file start fin; value = err }
end
