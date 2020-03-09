{
  [@@@coverage exclude_file]
  exception Error of (string * Lexing.position * Lexing.position)
  let lexeme_spanned lexbuf x = (x, Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)

  type token =
    | Open
    | Close
    | String of string
    | End
    | Skip
}

let white = [' ' '\t' ]
let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let id = ['!' '#'-'\'' '*'-':' '<'-'~'] (* All printable characters excluding '(', ')', ';' and '"'. *)

rule token l = parse
| white+                { Skip }
| '\n'                  { IlluaminateCore.Span.Lines.new_line l; Skip }
| ';' [^'\n']*          { Skip }

| '('                   { Open }
| ')'                   { Close }
| id+                   { String (Lexing.lexeme lexbuf) }
| '\"'                  { string (Buffer.create 17) l lexbuf }
| eof                   { End }
| _                     { raise (Error (lexeme_spanned lexbuf (Printf.sprintf "Unexpected character %S." (Lexing.lexeme lexbuf)))) }

and string value l = parse
| '\"'              { String (Buffer.contents value) }
| '\''              { Buffer.add_char value '\'';   string value l lexbuf }

| "\\a"             { Buffer.add_char value '\007'; string value l lexbuf }
| "\\b"             { Buffer.add_char value '\b';   string value l lexbuf }
| "\\f"             { Buffer.add_char value '\012'; string value l lexbuf }
| "\\n"             { Buffer.add_char value '\n';   string value l lexbuf }
| "\\r"             { Buffer.add_char value '\r';   string value l lexbuf }
| "\\v"             { Buffer.add_char value '\011'; string value l lexbuf }
| "\\t"             { Buffer.add_char value '\t';   string value l lexbuf }

| "\\\\"            { Buffer.add_char value '\\';   string value l lexbuf }
| "\\\""            { Buffer.add_char value '\"';   string value l lexbuf }
| "\\\'"            { Buffer.add_char value '\'';   string value l lexbuf }

| "\\x" ((hex hex?) as x)
                    { Buffer.add_char value ("0x" ^ x |> int_of_string |> char_of_int);
                      string value l lexbuf }
| "\\" ((digit digit? digit?) as x)
                    { Buffer.add_char value (int_of_string x |> char_of_int);
                      string value l lexbuf }

| [^'\\' '\"' '\'' '\n']+ as x
                    { Buffer.add_string value x;
                      string value l lexbuf }

| eof               { raise (Error (lexeme_spanned lexbuf "Expected '\"' at end of file.")) }
| '\n'              { raise (Error (lexeme_spanned lexbuf "Expected '\"' at end of line.")) }
| _                 { raise (Error (lexeme_spanned lexbuf (Printf.sprintf "Unexpected character %S in string." (Lexing.lexeme lexbuf)))) }
