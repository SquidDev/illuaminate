open Illuaminate
open IlluaminateCore

val parse :
  File_id.t ->
  Lexing.lexbuf ->
  (string Span.spanned * string Span.spanned) list * string Span.spanned
