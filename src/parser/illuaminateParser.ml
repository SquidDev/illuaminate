module Span = IlluaminateCore.Span
open Token
open Error
module I = Grammar.MenhirInterpreter
module Error = Error

type 'a located =
  { span : Span.t;
    token : 'a;
    line : int
  }

let lex_one lines (lexbuf : Lexing.lexbuf) =
  let start = lexbuf.lex_curr_p in
  let token = Lexer.token lines lexbuf in
  { token; span = Span.of_pos2 lines start lexbuf.lex_curr_p; line = start.pos_lnum }

let lex_leading lines lexbuf =
  let rec go xs =
    match lex_one lines lexbuf with
    | { token = Trivial value; span; _ } -> go ({ Span.value; span } :: xs)
    | { token = Token token; span; line } -> (List.rev xs, { token; span; line })
  in
  go []

let lex_trailing file lexbuf prev_line =
  let rec go xs =
    match lex_one file lexbuf with
    | { token = Trivial value; span; line } when line = prev_line -> go ({ Span.value; span } :: xs)
    | t -> (List.rev xs, t)
  in
  go []

let lex_token file lexbuf (next : lexer_token located) =
  let leading, { token; span = tok_span; line = tok_line } =
    match next with
    | { token = Trivial value; span; _ } ->
        let leading, t = lex_leading file lexbuf in
        ({ Span.value; span } :: leading, t)
    | { token = Token token; span; line } -> ([], { token; span; line })
  in
  match token with
  | EoF ->
      (* Just return the current "next" token (we won't inspect it after all, and an EOF token with
         no trailing data. *)
      (Token.make_token leading [] tok_span token, next)
  | _ ->
      let trailing, next = lex_trailing file lexbuf tok_line in
      (Token.make_token leading trailing tok_span token, next)

let parse start (file : Span.filename) (lexbuf : Lexing.lexbuf) =
  Span.Lines.using file lexbuf @@ fun lines ->
  let rec go last next checkpoint =
    match checkpoint with
    | I.InputNeeded _ ->
        let tok, next = lex_token lines lexbuf next in
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
        | None -> assert false)
    | I.Accepted x -> Ok x
    | I.Rejected -> assert false
  in
  try
    let first = lex_one lines lexbuf in
    start Lexing.dummy_pos |> go None first
  with Lexer.Error (err, start, fin) ->
    Error { Span.span = Span.of_pos2 lines start fin; value = err }

let program = parse Grammar.Incremental.program
let repl_exprs = parse Grammar.Incremental.repl_exprs

module Lexer = struct
  type token = lexer_token =
    | Token of IlluaminateCore.Token.t
    | Trivial of IlluaminateCore.Node.trivial

  let lex (file : Span.filename) (lexbuf : Lexing.lexbuf) :
      (token Span.spanned array, Error.t Span.spanned) result =
    Span.Lines.using file lexbuf @@ fun lines ->
    try
      let rec go xs =
        let { token; span; _ } = lex_one lines lexbuf in
        let xs = { Span.value = token; span } :: xs in
        match token with
        | Token EoF -> xs
        | _ -> go xs
      in
      go [] |> List.rev |> Array.of_list |> Result.ok
    with Lexer.Error (err, start, fin) ->
      Error { Span.span = Span.of_pos2 lines start fin; value = err }
end
