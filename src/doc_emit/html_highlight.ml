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

let emit ~input visit tree =
  let open Html.Default in
  (* TODO: Emit a true HTML node. Not sure how to do that elegantly though - we'd probably need to
     use a visitor within Emit instead. *)
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
  visit out tree;
  Format.pp_print_flush out ();
  raw (Buffer.contents res)

(** Highlight Lua as HTML code. *)
let lua input =
  let file = Span.Filename.mk "=input" in
  let program = Lexing.from_string input |> IlluaminateParser.program file
  and expr = lazy (Lexing.from_string input |> IlluaminateParser.repl_exprs file) in

  match (program, expr) with
  | Ok tree, _ -> emit ~input Emit.program tree
  | Error _, (lazy (Ok tree)) -> emit ~input Emit.repl_exprs tree
  | Error _, (lazy (Error _)) -> Html.Default.str input
