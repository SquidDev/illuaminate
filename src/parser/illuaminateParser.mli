module Span := IlluaminateCore.Span

(** An error which may occur when parsing. *)
module Error = Error

module Lexer : sig
  (** The type of tokens produced by the lexer. *)
  type token =
    | Token of IlluaminateCore.Token.t
    | Trivial of IlluaminateCore.Node.trivial

  (** Lex a file, producing a simple token stream. *)
  val lex :
    Span.filename -> Lexing.lexbuf -> (token Span.spanned array, Error.t Span.spanned) result
end

(** Parse a file, either producing a program or some syntax error. *)
val program :
  Span.filename -> Lexing.lexbuf -> (IlluaminateCore.Syntax.program, Error.t Span.spanned) result

(** Parse a list of expressions. *)
val repl_exprs :
  Span.filename -> Lexing.lexbuf -> (IlluaminateCore.Syntax.repl_exprs, Error.t Span.spanned) result
