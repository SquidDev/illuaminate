open IlluaminateCore

(** An error which may occur when parsing. *)
module Error : sig
  type t =
    | UnexpectedCharacter of string
    | UnterminatedString
    | SyntaxError of string
    | UnexpectedToken of Grammar.token * string

  val report : Error.t -> Span.t -> t -> unit
end

(** Parse a file, either producing a program or some syntax error. *)
val parse : Span.filename -> Lexing.lexbuf -> (Syntax.program, Error.t Span.spanned) result
