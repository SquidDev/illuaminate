(** An error which may occur when parsing. *)
module Error = Error

module Lexer : sig
  (** The type of tokens produced by the lexer. *)
  type token =
    | Token of string
    | Trivial of IlluaminateCore.Node.trivial

  (** Lex a file, producing a simple token stream. *)
  val lex :
    Illuaminate.File_id.t ->
    Lexing.lexbuf ->
    (token IlluaminateCore.Span.spanned array, Error.t) result
end

(** Parse a file, either producing a program or some syntax error. *)
val program :
  Illuaminate.File_id.t -> Lexing.lexbuf -> (IlluaminateCore.Syntax.program, Error.t) result

(** Parse a list of expressions. *)
val repl_exprs :
  Illuaminate.File_id.t -> Lexing.lexbuf -> (IlluaminateCore.Syntax.repl_exprs, Error.t) result
