(** Converts Menhir's error messages file (passed on stdin) into a list of test cases which can be
    passed to ./check-markdown.lua.

    This is used to generate ./parser_messages_spec.md: see the dune file in the parent directory.
*)

let token_name = function
  | "ADD" -> "+"
  | "AND" -> "and"
  | "BREAK" -> "break"
  | "CBRACE" -> "}"
  | "COLON" -> ":"
  | "COMMA" -> ","
  | "CONCAT" -> ".."
  | "CPAREN" -> ")"
  | "CSQUARE" -> "]"
  | "DIV" -> "/"
  | "DO" -> "do"
  | "DOT" -> "."
  | "DOTS" -> "..."
  | "DOUBLE_COLON" -> "::"
  | "ELSE" -> "else"
  | "ELSEIF" -> "elseif"
  | "END" -> "end"
  | "EQ" -> "=="
  | "EQUALS" -> "="
  | "FALSE" -> "false"
  | "FOR" -> "for"
  | "FUNCTION" -> "function"
  | "GE" -> ">="
  | "GOTO" -> "goto"
  | "GT" -> ">"
  | "IDENT" -> "xyz"
  | "IF" -> "if"
  | "IN" -> "in"
  | "LE" -> "<="
  | "LEN" -> "#"
  | "LOCAL" -> "local"
  | "LT" -> "<"
  | "MOD" -> "%"
  | "MUL" -> "*"
  | "NE" -> "~="
  | "NIL" -> "nil"
  | "NOT" -> "not"
  | "NUMBER" -> "123"
  | "OBRACE" -> "{"
  | "OPAREN" -> "("
  | "OR" -> "or"
  | "OSQUARE" -> "["
  | "POW" -> "^"
  | "REPEAT" -> "repeat"
  | "RETURN" -> "return"
  | "SEMICOLON" -> ";"
  | "STRING" -> "'abc'"
  | "SUB" -> "-"
  | "THEN" -> "then"
  | "TRUE" -> "true"
  | "UNTIL" -> "until"
  | "WHILE" -> "while"
  | "EOF" -> "--[[eof]]"
  | x -> x

let () =
  print_endline
    {|An exhaustive list of all error states in the parser, and the error messages we
generate for each one. This is _not_ a complete collection of all possible
errors, but is a useful guide for where we might be providing terrible messages.|};

  let inputs = ref [] in
  (try
     let re =
       Re.compile @@ Re.whole_string
       @@ Re.seq
            [ Re.group (Re.rep1 (Re.alt [ Re.lower; Re.char '_' ]));
              Re.str ": ";
              Re.group (Re.rep1 Re.any)
            ]
     in
     while true do
       let line = input_line stdin in
       match Re.exec_opt re line with
       | None -> ()
       | Some g -> inputs := (Re.Group.get g 1, Re.Group.get g 2) :: !inputs
     done
   with End_of_file -> ());
  let inputs =
    List.sort
      (fun (x1, x2) (y1, y2) ->
        match String.compare x1 y1 with
        | 0 -> String.compare x2 y2
        | n -> n)
      !inputs
  in

  let word = Re.(rep1 (alt [ upper; char '_' ])) |> Re.compile in
  Fun.flip List.iter inputs @@ fun (start, tokens) ->
  print_endline "";
  print_string "```lua";
  if start <> "program" then Printf.printf "{%s}" start;
  print_endline "";

  let tokens = Re.replace word ~f:(fun g -> token_name (Re.Group.get g 0)) tokens in
  print_endline tokens; print_endline "```"
