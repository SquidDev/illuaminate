{
  [@@@coverage exclude_file]
  open Pattern
  open Builder
}

rule main  = parse
  (* The initial rule is just used to handle leading **s. *)
  | "**" eof          { [] }
  | "**" '/'          { glob [Anything] true Empty lexbuf }
  | ""                { glob [] false Empty lexbuf }

and glob segments has_sep current = parse
  (* If we're at the end of the file, just append our trailing segment.
     We (somewhat incorrectly) treat f f/ and f/** the same - we treat
     them all as including this file/folder and everything inside. *)
  | eof               { mk (current **: segments) ~has_sep }
  | '/' eof           { mk (current **: segments) ~has_sep }
  | '/' "**" eof      { mk (current **: segments) ~has_sep }

  (* Directory separators: finish off the current node and continue. *)
  | '/'               { glob (current **: segments)             true Empty lexbuf }
  | '/' "**" '/'      { glob (Anything :: current **: segments) true Empty lexbuf }

  (* Otherwise, append to our current segment. *)
  | '\\' (_ as c)     { glob segments has_sep (add_char c current)              lexbuf }
  | '*'               { glob segments has_sep (add_re (Re.rep not_sep) current) lexbuf }
  | '?'               { glob segments has_sep (add_re not_sep current)          lexbuf }
  | (_ as c)          { glob segments has_sep (add_char c current)              lexbuf }
