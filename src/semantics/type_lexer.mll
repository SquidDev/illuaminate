{
  [@@@coverage exclude_file]
  open Type_grammar

  type error =
    | UnexpectedCharacter of string
    | UnterminatedString
    | SyntaxError of string
    | UnexpectedToken of token * string

  exception Error of (error * Lexing.position * Lexing.position)
  let lexeme_spanned lexbuf x = (x, Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
}

let white = [' ' '\t' ]

let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

let ident_head = ['a'-'z' 'A'-'Z' '_']
let ident_tail = ident_head | '_' | digit

rule token = parse
| white+                { token lexbuf }
| '\n'                  { Lexing.new_line lexbuf; token lexbuf }

| "false"    { FALSE    }
| "nil"      { NIL      }
| "true"     { TRUE     }

| "function" { FUNCTION }

| ","        { COMMA }
| ":"        { COLON }
| "."        { DOT }
| "..."      { DOTS }
| "="        { EQUALS }
| "|"        { PIPE }
| "?"        { ASKING }

| '(' { OPAREN } | ')' { CPAREN }
| '{' { OBRACE } | '}' { CBRACE }
| '[' { OSQUARE } | ']' { CSQUARE }

(* Numbers *)
| digit+ as i { (INT (int_of_string i)) }
| "0x" hex+ as i { (INT (int_of_string i)) }

| digit* '.' digit+ as i { (NUMBER (float_of_string i)) }
| digit* '.' digit+ ['E' 'e'] digit+ as i { (NUMBER (float_of_string i)) }
| digit* '.' digit+ ['E' 'e'] ['+' '-'] digit+ as i { (NUMBER (float_of_string i)) }

(* Identifiers *)
| ident_head ident_tail* as i { (IDENT i) }

| '\"'          { string (Buffer.create 17) '\"' lexbuf }
| '\''          { string (Buffer.create 17) '\'' lexbuf }
| '[' ('='* as x) '[' { long_string (Buffer.create 16) (String.length x) (fun x -> (STRING x)) lexbuf }

| eof { EOF }

| _ { raise (Error (lexeme_spanned lexbuf (UnexpectedCharacter (Lexing.lexeme lexbuf)))) }

and string buf c = parse
| '\"'              { if c = '\"' then (STRING (Buffer.contents buf))
                      else (Buffer.add_char buf '\"'; string buf c lexbuf) }
| '\''              { if c = '\'' then (STRING (Buffer.contents buf))
                      else (Buffer.add_char buf '\''; string buf c lexbuf) }

| "\\a"             { Buffer.add_char buf '\007'; string buf c lexbuf }
| "\\b"             { Buffer.add_char buf '\b'; string buf c lexbuf }
| "\\f"             { Buffer.add_char buf '\012'; string buf c lexbuf }
| "\\n"             { Buffer.add_char buf '\n'; string buf c lexbuf }
| "\\r"             { Buffer.add_char buf '\r'; string buf c lexbuf }
| "\\v"             { Buffer.add_char buf '\011'; string buf c lexbuf }
| "\\t"             { Buffer.add_char buf '\t'; string buf c lexbuf }

| "\\\\"            { Buffer.add_char buf '\\'; string buf c lexbuf }
| "\\\""            { Buffer.add_char buf '\"'; string buf c lexbuf }
| "\\\'"            { Buffer.add_char buf '\''; string buf c lexbuf }

| "\\x" ((hex hex?) as x)
  { Buffer.add_char buf ("0x" ^ x |> int_of_string |> char_of_int); string buf c lexbuf }
| "\\" ((digit digit? digit?) as x)
  { Buffer.add_char buf (int_of_string x |> char_of_int); string buf c lexbuf }

| [^'\\' '\"' '\'' '\n']+ as x { Buffer.add_string buf x; string buf c lexbuf }

| eof { raise (Error (lexeme_spanned lexbuf UnterminatedString)) }
| '\n' { raise (Error (lexeme_spanned lexbuf UnterminatedString)) }
| _ { raise (Error (lexeme_spanned lexbuf (UnexpectedCharacter (Lexing.lexeme lexbuf)))) }

and long_string buf eq term = parse
| [^']']+ as x      { Buffer.add_string buf x; long_string buf eq term lexbuf }
| ']' '='* ']' as x { if String.length x == eq + 2 then term (Buffer.contents buf)
                        else (Buffer.add_string buf x; long_string buf eq term lexbuf) }
| ']'               { Buffer.add_char buf ']'; long_string buf eq term lexbuf }
| eof               { raise (Error (lexeme_spanned lexbuf UnterminatedString)) }
