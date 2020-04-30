{
  [@@@coverage exclude_file]
  type line_start = Tag of string | NotTag | Eof
  type tag_stop = Stop | Separator | Unknown
  exception Error of string * Lexing.position
}

let white = [ ' ' '\t' ]
let white_nl = white | '\n'

let any = ['!' - '~']
let alpha = ['a'-'z' 'A'-'Z']

rule white   = parse | white*    { () }
and white_nl = parse | white_nl* { () }
and until_line = parse | [^'\n']* as x { x }
and line = parse | '\n' { true } | "" { false }

and maybe_tag = parse
| "@" (alpha+ as x)    { Tag x }
| eof                  { Eof }
| ""                   { NotTag }

and tag_start = parse
| '['                   { true }
| ""                    { false }

and tag_stop = parse
| ']'                   { Stop }
| ','                   { Separator }
| ""                    { Unknown }

and word buf = parse
| ['(' '[' '{'] as x    { Buffer.add_char buf x; inner_string (word buf) buf [(x, lexbuf.lex_curr_p)] lexbuf }
| any as x              { Buffer.add_char buf x; word buf lexbuf }
| ""                    { Buffer.contents buf }

and key = parse
| (alpha+ as x) '='     { Some x }
| ""                    { None }

and value buf = parse
| ['(' '[' '{'] as x    { Buffer.add_char buf x; inner_string (value buf) buf [(x, lexbuf.lex_curr_p)] lexbuf }
| (any # [',' ']']) as x { Buffer.add_char buf x; value buf lexbuf }
| ""                    { Buffer.contents buf }

and inner_string k buf stack = parse
| ['(' '[' '{'] as x
  { Buffer.add_char buf x;
    inner_string k buf ((x, lexbuf.lex_curr_p) :: stack) lexbuf }

| [')' ']' '}'] as x
  { match x, stack with
    | _, [] -> failwith "Impossible: empty stack!"
    | ')', (('(', _) :: stack)
    | ']', (('[', _) :: stack)
    | '}', (('{', _) :: stack) ->
       Buffer.add_char buf x;
       ( match stack with
         | [] -> k lexbuf
         | _ -> inner_string k buf stack lexbuf )
    | _, (_ :: _) -> Buffer.add_char buf x; inner_string k buf stack lexbuf }

| _ as x                { Buffer.add_char buf x; inner_string k buf stack lexbuf }
| eof
  { (* TODO: Warn on unclosed terms. *)
    Buffer.contents buf }
