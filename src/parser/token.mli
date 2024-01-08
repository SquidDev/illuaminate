open IlluaminateCore

(** The tokens emitted by the lexer. *)
type lexer_token =
  | Token of Token.t
  | Trivial of Node.trivial

(** Make a parser token from the leading/trailing trivia, a span and token. *)
val make_token :
  Node.trivial Span.spanned list ->
  Node.trivial Span.spanned list ->
  Span.t ->
  Token.t ->
  Grammar.token

(** Convert a grammar token back into a normal one. *)
val to_string : Grammar.token -> string
