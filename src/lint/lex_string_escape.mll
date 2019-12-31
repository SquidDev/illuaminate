let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

rule string errors c = parse
| '\"'              { if c = '\"' then Some errors else string errors c lexbuf }
| '\''              { if c = '\'' then Some errors else string errors c lexbuf }

| "\\" ['a' 'b' 'n' 'r' 'v' 't' '\\' '\"' '\'' ] { string errors c lexbuf }
| "\\x" (hex hex?)               { string errors c lexbuf }
| "\\" (digit digit? digit?)     { string errors c lexbuf }

| "\\" ([^ '\n'] as x)           { string ((x, Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf) :: errors) c lexbuf }
| [^'\\' '\"' '\'' '\n']+        { string errors c lexbuf }

(* We should never have malformed strings, but let's handle this semi-gracefully. *)
| eof                            { None }
| '\n'                           { None }
| _                              { None }

(** Consumes the first character of a string, and then delegates off to string. *)
and string_of = parse
| [^ '\n'] as c                  { string [] c lexbuf }
| eof                            { None }
| _                              { None}
