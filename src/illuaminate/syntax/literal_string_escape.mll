{
  open Literal_string_component
}

let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

rule component = parse
| '\"'                           { Quote '\"' }
| '\''                           { Quote '\'' }

| "\\a"                          { Escape '\007' }
| "\\b"                          { Escape '\b'   }
| "\\f"                          { Escape '\012' }
| "\\n"                          { Escape '\n'   }
| "\\r"                          { Escape '\r'   }
| "\\v"                          { Escape '\011' }
| "\\t"                          { Escape '\t'   }
| "\\\\"                         { Escape '\\'   }
| "\\\""                         { Escape '\"'   }
| "\\\'"                         { Escape '\''   }
| "\\x" (hex as x) (hex as y)    { hex_escape x y }
| "\\" ((digit digit? digit?) as x) { decimal_escape x}
| "\\u{" (hex+ as x) "}"         { unicode_escape x }
| "\\z" ['\000'-' ']             { Zap }

| "\\" ([^ '\n'] as x)           { Unknown_escape x }
| [^'\\' '\"' '\'' '\n']+        { Segment }

(* We should never have malformed strings, but let's handle this semi-gracefully. *)
| eof                            { raise Malformed }
| '\n'                           { raise Malformed }
| _                              { raise Malformed }
