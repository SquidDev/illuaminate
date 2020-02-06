type t =
  | UnexpectedCharacter of string
  | UnterminatedString
  | SyntaxError of string
  | UnexpectedToken of Grammar.token * string

val report : IlluaminateCore.Error.t -> IlluaminateCore.Span.t -> t -> unit

val pp : Format.formatter -> t -> unit
