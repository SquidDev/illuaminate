{
  [@@@coverage exclude_file]
  exception Error of string * Lexing.position

  let new_line = IlluaminateCore.Span.Lines.new_line

  type header = End | Blank | Key of string | Malformed
}

let field = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']
let space = [ ' ' '\t' ]

rule start l = parse
| '-' '-' '-'+ '\n'     { new_line l; true }
| ""                    { false }

(* Like a YAML parser, but bad. *)
and header l = parse
| (field+ as key) ':' space* { Key key }
| '\n'                  { new_line l; Blank }
| '-' '-' '-'+ '\n'     { new_line l; End }
| eof                   { End }
| ""                    { Malformed }

and header_contents l = parse
| [^'\n']* as value { new_line l; value }

and body contents l = parse
| [^'\n']+ as c         { Buffer.add_string contents c; body contents l lexbuf }
| '\n'                  { new_line l; Buffer.add_char contents '\n'; body contents l lexbuf; }
| eof                   { Buffer.contents contents }
