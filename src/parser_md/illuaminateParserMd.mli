open IlluaminateCore

val parse :
  Span.filename ->
  Lexing.lexbuf ->
  (string Span.spanned * string Span.spanned) list * string Span.spanned
