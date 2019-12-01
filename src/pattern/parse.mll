{
  open Pattern
  let mk re has_sep =
    let re = List.rev re |> Re.seq in
    { pattern = Re.(seq [ start; re; stop ] |> compile); absolute = has_sep }

  let not_sep = Re.(diff any (char '/'))

  (* This terrible regex allows for any number of directory entries after this one. *)
  (* ((^|/).* )? *)
  let more_dirs = Re.(opt (seq [ alt [ start; char '/' ] ; rep any ]))
}

rule main = parse
  | '/'               { glob [] true  lexbuf }
  | ""                { glob [] false lexbuf }

and glob out has_sep = parse
  | eof               { mk (more_dirs :: out)              has_sep }
  | '/' eof           { mk Re.(rep any :: char '/' :: out) has_sep } (* /.* *)

  | '\\' (_ as c)     { glob (Re.char c         :: out) has_sep lexbuf }
  | "**"              { glob (Re.(rep any)      :: out) has_sep lexbuf }
  | '*'               { glob (Re.rep not_sep    :: out) has_sep lexbuf }
  | '?'               { glob (not_sep           :: out) has_sep lexbuf }
  | '/'               { glob (Re.char '/'       :: out) true    lexbuf }
  | (_ as c)          { glob (Re.char c         :: out) has_sep lexbuf }

{
  let parse str : t = Lexing.from_string ~with_positions:false str |> main
}
