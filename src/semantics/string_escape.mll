{
[@@@coverage exclude_file]
open IlluaminateCore

type component =
  | Segment of string
  | Escape of string * char
  | Malformed of char * Span.t
  | Quote of char

let pos (span : Span.t) lexbuf =
  let open Lens in
  span
  |> (Span.start_col ^= (Span.start_col.get span + (Lexing.lexeme_start_p lexbuf).pos_cnum))
     % (Span.finish_col ^= (Span.start_col.get span + (Lexing.lexeme_end_p lexbuf).pos_cnum - 1))

}

let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

rule string xs c s = parse
| '\"'                           { if c = '\"' then Some (List.rev xs) else string (Quote '\"' :: xs) c s lexbuf }
| '\''                           { if c = '\'' then Some (List.rev xs) else string (Quote '\''  :: xs) c s lexbuf }

| "\\a"                          { string (Escape ("\\a", '\007') :: xs) c s lexbuf }
| "\\b"                          { string (Escape ("\\b", '\b')   :: xs) c s lexbuf }
| "\\f"                          { string (Escape ("\\f", '\012') :: xs) c s lexbuf }
| "\\n"                          { string (Escape ("\\n", '\n')   :: xs) c s lexbuf }
| "\\r"                          { string (Escape ("\\r", '\r')   :: xs) c s lexbuf }
| "\\v"                          { string (Escape ("\\v", '\011') :: xs) c s lexbuf }
| "\\t"                          { string (Escape ("\\t", '\t')   :: xs) c s lexbuf }
| "\\\\"                         { string (Escape ("\\\\", '\\')  :: xs) c s lexbuf }
| "\\\""                         { string (Escape ("\\\"", '\"')  :: xs) c s lexbuf }
| "\\\'"                         { string (Escape ("\\\'", '\'')  :: xs) c s lexbuf }
| "\\x" ((hex hex?) as x)        { string (Escape ("\\x" ^ x, "0x" ^ x |> int_of_string |> char_of_int) :: xs) c s lexbuf }
| "\\" ((digit digit? digit?) as x) { string (Escape ("\\" ^ x, int_of_string x |> char_of_int) :: xs) c s lexbuf }

| "\\" ([^ '\n'] as x)           { string (Malformed (x, pos s lexbuf) :: xs) c s lexbuf }
| [^'\\' '\"' '\'' '\n']+ as x   { string (Segment x :: xs) c s lexbuf }

(* We should never have malformed strings, but let's handle this semi-gracefully. *)
| eof                            { None }
| '\n'                           { None }
| _                              { None }

(** Consumes the first character of a string, and then delegates off to string. *)
and string_of s = parse
| [^ '\n'] as c                  { string [] c s lexbuf }
| eof                            { None }
| _                              { None }
