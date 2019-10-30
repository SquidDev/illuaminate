{
  open Error
  open Token
  open IlluaminateCore.Node
  open IlluaminateCore.Token

  exception Error of (Error.t * Lexing.position * Lexing.position)
  let lexeme_spanned lexbuf x = (x, Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
}

let white = [' ' '\t' ]


let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

let ident_head = ['a'-'z' 'A'-'Z' '_']
let ident_tail = ident_head | '_' | digit

rule token = parse
| white+ as x           { Trivial (Whitespace x) }
| '\n'                  { Lexing.new_line lexbuf; Trivial (Whitespace "\n") }
| "--[" ('='* as x) '[' { long_string (Buffer.create 16) (String.length x) (fun c -> Trivial (BlockComment (String.length x, c))) lexbuf }
| "--" ([^'\n''['] [^'\n']* as x) { Trivial (LineComment x) } (* Fix this horrible hack *)
| "--"                  { Trivial (LineComment "") }


| "and"      { Token And      }
| "break"    { Token Break    }
| "do"       { Token Do       }
| "else"     { Token Else     }
| "elseif"   { Token ElseIf   }
| "end"      { Token End      }
| "false"    { Token False    }
| "for"      { Token For      }
| "function" { Token Function }
| "if"       { Token If       }
| "in"       { Token In       }
| "local"    { Token Local    }
| "nil"      { Token Nil      }
| "not"      { Token Not      }
| "or"       { Token Or       }
| "repeat"   { Token Repeat   }
| "return"   { Token Return   }
| "then"     { Token Then     }
| "true"     { Token True     }
| "until"    { Token Until    }
| "while"    { Token While    }

| ":"        { Token Colon }
| ","        { Token Comma }
| "."        { Token Dot }
| "..."      { Token Dots }
| "="        { Token Equals }
| ";"        { Token Semicolon }

| '(' { Token OParen }  | ')' { Token CParen }
| '{' { Token OBrace }  | '}' { Token CBrace }
| '[' { Token OSquare } | ']' { Token CSquare }

| '+'  { Token Add }
| '-'  { Token Sub }
| '*'  { Token Mul }
| '/'  { Token Div }
| '^'  { Token Pow }
| '%'  { Token Mod }
| ".." { Token Concat }
| "==" { Token Eq }
| "~=" { Token Ne }
| "<"  { Token Lt }
| "<=" { Token Le }
| ">"  { Token Gt }
| ">=" { Token Ge }
| '#'  { Token Len }

(* Numbers *)
| digit+    as i { Token (Int (int_of_string i, i)) }
| "0x" hex+ as i { Token (Int (int_of_string i, i)) }

| digit* '.' digit+ as i { Token (Number (float_of_string i, i)) }
| digit* '.' digit+ ['E' 'e'] digit+ as i { Token (Number (float_of_string i, i)) }
| digit* '.' digit+ ['E' 'e'] ['+' '-'] digit+ as i { Token (Number (float_of_string i, i)) }

(* Identifiers *)
| ident_head ident_tail* as i { Token (Ident i) }

| '\"'          { string (Buffer.create 17) '\"' lexbuf }
| '\''          { string (Buffer.create 17) '\'' lexbuf }
| '[' ('='* as x) '[' { long_string (Buffer.create 16) (String.length x) (fun x -> Token (String (x, x))) lexbuf }
(* TODO: This is utterly wrong. *)

| eof { Token EoF }

| _ { raise (Error (lexeme_spanned lexbuf (UnexpectedCharacter (Lexing.lexeme lexbuf)))) }

and string buf c = parse
| '\"'              { if c = '\"' then Token (String ("", Buffer.contents buf))
                      else (Buffer.add_char buf '\"'; string buf c lexbuf) }
| '\''              { if c = '\'' then Token (String ("", Buffer.contents buf))
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
