{
  open Pattern
  let quot_char c = String.make 1 c |> Str.quote
  let mk out has_sep =
    Buffer.add_string out "$";
    { pattern = Buffer.contents out |> Str.regexp; absolute = has_sep }
}

rule main out = parse
  | '/'               { glob out true  lexbuf }
  | ""                { glob out false lexbuf }

and glob out has_sep = parse
                      (* This terrible regex allows for any number of directory entries after this one. *)
  | eof               { Buffer.add_string out "\\(\\(^\\|/\\).*\\)?"; mk out has_sep }
  | '/' eof           { Buffer.add_string out "/.*"; mk out has_sep }

  | '\\' (_ as c)     { Buffer.add_string out (quot_char c); glob out has_sep lexbuf }
  | "**"              { Buffer.add_string out ".*";          glob out has_sep lexbuf }
  | '*'               { Buffer.add_string out "[^/]*";       glob out has_sep lexbuf }
  | '?'               { Buffer.add_string out "[^/]";        glob out has_sep lexbuf }
  | '/'               { Buffer.add_char   out '/';           glob out true    lexbuf }
  | (_ as c)          { Buffer.add_string out (quot_char c); glob out has_sep lexbuf }

{
  let parse str : t =
    Lexing.from_string ~with_positions:false str
    |> main (Buffer.create (String.length str))
}
