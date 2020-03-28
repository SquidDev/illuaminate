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

rule token = parse
| white+                { Skip }
| '\n'                  { Lexing.new_line lexbuf; Skip }
| ';' [^'\n']*          { Skip }

| '('                   { Open }
| ')'                   { Close }
| id+                   { String (Lexing.lexeme lexbuf) }
| '\"'                  { string (Buffer.create 17) lexbuf }
| eof                   { End }
| _                     { raise (Error (lexeme_spanned lexbuf (Printf.sprintf "Unexpected character %S." (Lexing.lexeme lexbuf)))) }

and string value = parse
| '\"'              { String (Buffer.contents value) }
| '\''              { Buffer.add_char value '\'';   string value lexbuf }

| "\\a"             { Buffer.add_char value '\007'; string value lexbuf }
| "\\b"             { Buffer.add_char value '\b';   string value lexbuf }
| "\\f"             { Buffer.add_char value '\012'; string value lexbuf }
| "\\n"             { Buffer.add_char value '\n';   string value lexbuf }
| "\\r"             { Buffer.add_char value '\r';   string value lexbuf }
| "\\v"             { Buffer.add_char value '\011'; string value lexbuf }
| "\\t"             { Buffer.add_char value '\t';   string value lexbuf }

| "\\\\"            { Buffer.add_char value '\\';   string value lexbuf }
| "\\\""            { Buffer.add_char value '\"';   string value lexbuf }
| "\\\'"            { Buffer.add_char value '\'';   string value lexbuf }

| "\\x" ((hex hex?) as x)
                    { Buffer.add_char value ("0x" ^ x |> int_of_string |> char_of_int);
                      string value lexbuf }
| "\\" ((digit digit? digit?) as x)
                    { Buffer.add_char value (int_of_string x |> char_of_int);
                      string value lexbuf }

| [^'\\' '\"' '\'' '\n']+ as x
                    { Buffer.add_string value x;
                      string value lexbuf }

| eof               { raise (Error (lexeme_spanned lexbuf "Expected '\"' at end of file.")) }
| '\n'              { raise (Error (lexeme_spanned lexbuf "Expected '\"' at end of line.")) }
| _                 { raise (Error (lexeme_spanned lexbuf (Printf.sprintf "Unexpected character %S in string." (Lexing.lexeme lexbuf)))) }
