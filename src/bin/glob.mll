{
  let quot_char c = String.make 1 c |> Str.quote
}

rule glob out = parse
  | eof               { Buffer.contents out |> Str.regexp }
  | '\\' (_ as c)     { Buffer.add_string out (quot_char c); glob out lexbuf }
  | "**"              { Buffer.add_string out ".*";          glob out lexbuf }
  | '*'               { Buffer.add_string out "[^/]*";       glob out lexbuf }
  | '?'               { Buffer.add_string out "[^/]";        glob out lexbuf }
  | (_ as c)          { Buffer.add_string out (quot_char c); glob out lexbuf }

{
  let parse str =
    Lexing.from_string ~with_positions:false str
    |> glob (Buffer.create (String.length str))
}
