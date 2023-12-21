{
  [@@@coverage exclude_file]
  open Error
  open Token
  open IlluaminateCore.Node
  open IlluaminateCore.Token
  let new_line = IlluaminateCore.Span.Lines.new_line

  exception Error of (Error.t * Lexing.position * Lexing.position)
  let lexeme_spanned lexbuf x = (x, Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)

  let buffer_with len char =
    let b = Buffer.create len in
    Buffer.add_char b char;
    b

  let mk_long_comment eqs c = Trivial (BlockComment (eqs, c))
  let mk_long_string eqs c =
    let eqs = String.make eqs '=' in
    Token (String ("[" ^ eqs ^ "[" ^ c ^ "]" ^ eqs ^ "]"))
}

let white = [' ' '\t']


let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let number = digit | ['E' 'e'] ['+' '-']? | '.'

let ident_head = ['a'-'z' 'A'-'Z' '_']
let ident_tail = ident_head | '_' | digit

rule token l = parse
| white+ as x           { Trivial (Whitespace x) }
| '\n'                  { new_line l; Trivial (Whitespace "\n") }
| '\r' '\n'             { new_line l; Trivial (Whitespace "\r\n") }
| "--[" ('='* as x) '[' { long_string (Buffer.create 16) (String.length x) mk_long_comment l lexbuf }
(* We split line comments into two parts. Otherwise "--[^\n]*" would match "--[[foo]]". *)
| "--"                  { line_comment lexbuf }

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
| "::"       { Token Double_colon }
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
| "0x" hex+ as i         { Token (Number i) }
| digit+ as i            { Token (Number i) }
| digit number* as i     { Token (Number i) }
| '.' digit number* as i { Token (Number i) }

(* Identifiers *)
| ident_head ident_tail* as i { Token (Ident i) }

| '\"'          { string (buffer_with 17 '\"') '\"' lexbuf }
| '\''          { string (buffer_with 17 '\'') '\'' lexbuf }
| '[' ('='* as x) '[' { long_string (Buffer.create 16) (String.length x) mk_long_string l lexbuf }

| eof { Token EoF }

| _ { raise (Error (lexeme_spanned lexbuf (Unexpected_character (Lexing.lexeme lexbuf)))) }

and string contents c = parse
| '\"'              { Buffer.add_char contents '\"';
                      if c = '\"' then Token (String (Buffer.contents contents))
                      else string contents c lexbuf }
| '\''              { Buffer.add_char contents '\'';
                      if c = '\'' then Token (String (Buffer.contents contents))
                      else string contents c lexbuf }

| "\\a"             { Buffer.add_string contents "\\a"; string contents c lexbuf }
| "\\b"             { Buffer.add_string contents "\\b"; string contents c lexbuf }
| "\\f"             { Buffer.add_string contents "\\f"; string contents c lexbuf }
| "\\n"             { Buffer.add_string contents "\\n"; string contents c lexbuf }
| "\\r"             { Buffer.add_string contents "\\r"; string contents c lexbuf }
| "\\v"             { Buffer.add_string contents "\\v"; string contents c lexbuf }
| "\\t"             { Buffer.add_string contents "\\t"; string contents c lexbuf }

| "\\x" ((hex hex?) as x)
                    { Buffer.add_string contents "\\x"; Buffer.add_string contents x;
                      string contents c lexbuf }
| "\\" ((digit digit? digit?) as x)
                    { Buffer.add_char contents '\\'; Buffer.add_string contents x;
                      string contents c lexbuf }
| "\\u{" (hex+ as x) "}"
  { Buffer.add_string contents "\\u{"; Buffer.add_string contents x; Buffer.add_string contents "}";
    string contents c lexbuf }

| "\\" ([^ '\r' '\n'] as x)
                    { Buffer.add_char contents '\\'; Buffer.add_char contents x;
                      string contents c lexbuf }

| [^'\\' '\"' '\'' '\n']+ as x
                    { Buffer.add_string contents x;
                      string contents c lexbuf }

| eof { raise (Error (lexeme_spanned lexbuf Unterminated_string)) }
| '\r' { raise (Error (lexeme_spanned lexbuf Unterminated_string)) }
| '\n' { raise (Error (lexeme_spanned lexbuf Unterminated_string)) }
| _ { raise (Error (lexeme_spanned lexbuf (Unexpected_character (Lexing.lexeme lexbuf)))) }

and long_string buf eqs term l = parse
| [^']' '\r' '\n']+ as x { Buffer.add_string buf x;              long_string buf eqs term l lexbuf }
| ']' '='* ']' as x      { if String.length x == eqs + 2
                           then term eqs (Buffer.contents buf)
                           else (Buffer.add_string buf x;        long_string buf eqs term l lexbuf) }
| ']'                    { Buffer.add_char buf ']';              long_string buf eqs term l lexbuf }
| '\n'                   { Buffer.add_char buf '\n'; new_line l; long_string buf eqs term l lexbuf }
| '\r' '\n'              { Buffer.add_string buf "\r\n"; new_line l; long_string buf eqs term l lexbuf }
| eof                    { raise (Error (lexeme_spanned lexbuf Unterminated_string)) }

and line_comment = parse
| [^'\r' '\n']* as x     { Trivial (LineComment x) }
