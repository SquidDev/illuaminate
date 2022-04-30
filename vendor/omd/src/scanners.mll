
rule table_row buffer all_blank has_row n xs = parse
| eof                   { all_blank, has_row, n, xs }
| '|'
  { if n = 0 && all_blank then (
      (* If we've just had whitespace beforehand and we're the first column,
         don't add it as a column. *)
      Buffer.clear buffer;
      table_row buffer true true n xs lexbuf
    ) else
      let contents = Buffer.contents buffer |> String.trim in
      Buffer.clear buffer;
      table_row buffer true true (n + 1) (contents :: xs) lexbuf }
| '\\' '|'
  { Buffer.add_char buffer '|';
    table_row buffer false has_row n xs lexbuf }
| '\\' _
  { Buffer.add_char buffer '\\'; Buffer.add_char buffer (Lexing.lexeme_char lexbuf 1);
    table_row buffer false has_row n xs lexbuf }

| [' ''\t' '\b']
  { Buffer.add_char buffer (Lexing.lexeme_char lexbuf 0);
    table_row buffer all_blank has_row n xs lexbuf }
| _
  { Buffer.add_char buffer (Lexing.lexeme_char lexbuf 0);
    table_row buffer false has_row n xs lexbuf }
