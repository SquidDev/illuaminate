{
[@@@coverage exclude_file]
type specifier =
  | Eof
  | Unknown of char
  | Known of string * char
  | Raw of string
}

let digit = ['0'-'9']
let hex = ['0'-'9' 'a'-'f' 'A'-'F']

let specifier = [ 'd' 'i' 'u' 'o' 'x' 'X' 'f' 'F' 'e' 'E' 'g' 'G' 'a' 'A' 'c' 's' 'p' 'n' '%' 'q' ]
let flags = [ '-' '+' ' ' '#' '0' ]

rule format specs = parse
| eof                            { List.rev specs }
| [^ '%']+ as x                  { format (Raw x :: specs) lexbuf }

| '%' (flags* digit* ('.' digit+)? as x) (specifier as c)
  { format (Known (x, c) :: specs) lexbuf }
| '%' (_ as x)
  { format (Unknown x :: specs) lexbuf }
| '%' eof
  { format (Eof :: specs) lexbuf }
