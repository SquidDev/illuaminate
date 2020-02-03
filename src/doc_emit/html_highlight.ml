open IlluaminateCore

let grammar_class = function
  | Emit.Keyword -> "keyword"
  | LiteralKeyword -> "literal-kw"
  | OperatorKeyword -> "operator-kw"
  | Symbol -> "symbol"
  | Identifier -> "ident"
  | String -> "string"
  | Number -> "number"
  | Comment -> "comment"

(** Highlight Lua as HTML code. *)
let lua input =
  let open Html.Default in
  match
    IlluaminateParser.parse { Span.name = "=input"; path = "=input" } (Lexing.from_string input)
  with
  | Error _ -> str input
  | Ok tree ->
      (* TODO: Emit a true HTML node. Not sure how to do that elegantly though - we'd probably need
         to use a visitor within Emit instead. *)
      let res = Buffer.create (String.length input) in
      let out = Format.formatter_of_buffer res in
      Format.pp_set_mark_tags out true;
      Format.pp_set_margin out 120;
      Format.pp_set_formatter_stag_functions out
        { mark_open_stag =
            (function
            | Emit.Token t -> Printf.sprintf "<span class=\"%s\">" (grammar_class t)
            | _ -> "");
          mark_close_stag =
            (function
            | Emit.Token _ -> "</span>"
            | _ -> "");
          print_open_stag = (fun _ -> ());
          print_close_stag = (fun _ -> ())
        };
      Emit.program out tree;
      Format.pp_print_flush out ();
      raw (Buffer.contents res)
