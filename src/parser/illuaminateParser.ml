module Span = IlluaminateCore.Span
module I = Grammar.MenhirInterpreter
module PE = Lrgrep_runtime.Interpreter (Parse_errors.Table_error_message) (I)
module Error = Error

type 'a located =
  { span : Span.t;
    start : Lexing.position;
    finish : Lexing.position;
    token : 'a
  }

let lex_one lines (lexbuf : Lexing.lexbuf) =
  let start = lexbuf.lex_curr_p in
  let token = Lexer.token lines lexbuf in
  let finish = lexbuf.lex_curr_p in
  { token; span = Span.of_pos2 lines start lexbuf.lex_curr_p; start; finish }

let lex_leading lines lexbuf =
  let rec go xs =
    match lex_one lines lexbuf with
    | { token = Trivial value; span; _ } -> go ({ Span.value; span } :: xs)
    | { token = Token token; _ } as rest -> (List.rev xs, { rest with token })
  in
  go []

let lex_trailing file lexbuf prev_line =
  let rec go xs =
    match lex_one file lexbuf with
    | { token = Trivial value; span; start; _ } when start.pos_lnum = prev_line ->
        go ({ Span.value; span } :: xs)
    | t -> (List.rev xs, t)
  in
  go []

let lex_token file lexbuf (next : Token.lexer_token located) =
  let leading, { token; span = tok_span; start; finish } =
    match next with
    | { token = Trivial value; span; _ } ->
        let leading, t = lex_leading file lexbuf in
        ({ Span.value; span } :: leading, t)
    | { token = Token token; _ } as rest -> ([], { rest with token })
  in
  match token with
  | EoF ->
      (* Just return the current "next" token (we won't inspect it after all, and an EOF token with
         no trailing data. *)
      (Token.make_token leading [] tok_span token, start, finish, next)
  | _ ->
      let trailing, next = lex_trailing file lexbuf start.pos_lnum in
      (Token.make_token leading trailing tok_span token, start, finish, next)

let get_error_message token ~pre_env ~post_env =
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
  let rec go env token token_start token_end next = function
    | I.InputNeeded env as checkpoint -> go_input env checkpoint next
    | (I.Shifting _ | I.AboutToReduce _) as checkpoint ->
        I.resume checkpoint |> go env token token_start token_end next
    | I.HandlingError post_env ->
        let message = get_error_message (token, token_start, token_end) ~pre_env:env ~post_env in
        Error { Error.file; position_map; message }
    | I.Accepted x -> Ok x
    | I.Rejected -> assert false
  and go_input env checkpoint token =
    let token, start, finish, next = lex_token lines lexbuf token in
    I.offer checkpoint (token, start, finish) |> go env token start finish next
  in
  try
    match start Lexing.dummy_pos with
    | I.InputNeeded env as checkpoint -> go_input env checkpoint (lex_one lines lexbuf)
    | _ -> assert false
  with Lexer.Error message -> Error { file; position_map; message }

let program = parse Grammar.Incremental.program
let repl_exprs = parse Grammar.Incremental.repl_exprs

module Lexer = struct
  type token = Token.lexer_token =
    | Token of IlluaminateCore.Token.t
    | Trivial of IlluaminateCore.Node.trivial

  let lex (file : Illuaminate.File_id.t) (lexbuf : Lexing.lexbuf) =
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
    with Lexer.Error message ->
      Error { Error.file; position_map = Span.Lines.position_map lines; message }
end
