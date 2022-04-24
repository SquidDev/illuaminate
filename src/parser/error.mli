open IlluaminateCore

type t =
  | UnexpectedCharacter of string
  | UnterminatedString
  | SyntaxError of string
  | UnexpectedToken of Grammar.token * string

val report : Error.t -> Span.t -> t -> unit
val tag : Error.Tag.t
val pp : Format.formatter -> t -> unit
