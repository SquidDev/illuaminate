(** A node which has a start and end position. *)
type 'a positioned = 'a * Lexing.position * Lexing.position

(** A token with a position. *)
type token = Grammar.token positioned

(** A span of ranges, equivalent to [unit positioned]. *)
type span = Lexing.position * Lexing.position

type message =
  | Unexpected_character of
      { position : Lexing.position;
        character : string
      }  (** An unexpected character was used.*)
  | Unterminated_string of
      { start : Lexing.position;
        position : Lexing.position
      }  (** A string which ends without a closing quote.*)
  | Unexpected_token of
      { token : token;  (** The unexpected token in question. *)
        message : string  (** The parser error, daken from [messages.txt]. *)
      }  (** A fallback error when we can't produce anything more useful.*)
  | Table_key_equals of token  (** [=] was used after an expression inside a table. *)
  | Use_double_equals of token  (** [=] was used in an expression context. *)
  | Unclosed_brackets of
      { open_ : IlluaminateCore.Syntax.token positioned;  (** The opening parenthesis. *)
        token : token  (** The current token. *)
      }  (** A parenthesised expression was started but not closed. *)
  | Missing_table_comma of
      { token : token;  (** The current token. *)
        comma_pos : Lexing.position  (** The position where we expected the comma. *)
      }  (** A missing comma between tokens. *)
  | Trailing_call_comma of
      { comma : IlluaminateCore.Syntax.token positioned;  (** The [,] token*)
        token : token  (** The closing [)]. *)
      }  (** A superflous [,] in a function call. *)
  | Local_function_dot of
      { local : IlluaminateCore.Syntax.token positioned;  (** The [local] token *)
        token : token  (** The invalid [.] token. *)
      }  (** [local function] was used with a table identifier.*)
  | Standalone_name of token  (** A statement of the form [x.y]*)
  | Standalone_names of token  (** A statement of the form [x.y, z]*)
  | Standalone_name_call of Lexing.position
      (** A statement of the form [x.y]. This is similar to {!Standalone_name}, but when the next
          token is on another line. *)
  | Expected_then of
      { if_ : IlluaminateCore.Syntax.token positioned;  (** The [if] or [elseif] token. *)
        pos : span  (** The current position. *)
      }  (** [then] was expected. *)
  | Expected_end of
      { start : span;  (** The start position of this block. *)
        token : token  (** The current token. *)
      }  (** [end] was expected. *)
  | Unexpected_end of token  (**An unexpected [end] in a statement. *)
  | Unfinished_label of
      { start : IlluaminateCore.Syntax.token positioned;  (** The opening [::]. *)
        token : token  (** The current token. *)
      }  (** A label declaration was started but not closed. *)
  | Expected_statement of token
      (** A fallback error when we expected a statement but received another token. *)
  | Expected_function_args of token  (** Expected `(` to open our function arguments. *)
  | Expected_expression of token
      (** A fallback error when we expected an expression but received another token. *)
  | Expected_var of token
      (** A fallback error when we expected a variable but received another token. *)

type t =
  { message : message;
    file : Illuaminate.File_id.t;
    position_map : Illuaminate.Position_map.t
  }

let to_error { message; file; position_map } =
  (* Pretty-printers *)
  let pp_code pp = Fmt.styled `Underline (fun o -> Fmt.fmt "`%a`" o pp) in
  let pp_code_s = pp_code Format.pp_print_string in
  let pp_token out ((t : Grammar.token), _, _) =
    match t with
    | EOF _ -> Format.pp_print_string out "end of file"
    | IDENT _ -> Format.pp_print_string out "identifier"
    | STRING _ -> Format.pp_print_string out "string"
    | NUMBER _ -> Format.pp_print_string out "number"
    | token -> pp_code_s out (Token.to_string token)
  in

  (* Position functions *)
  let lex_pos s f = Illuaminate.Error.Position.of_lex_pos ~file ~position_map s f in
  let lex_pos1 p = lex_pos p p in
  let posd (_, s, f) = lex_pos s f in
  let span (s, f) = lex_pos s f in

  (* Message helpers. *)
  let fmt msg out () = msg (Fmt.pf out) in
  let mk = Illuaminate.Error.v ~code:"parse:syntax-error" ~severity:Error ~tags:[] in
  let report ~pos msg = mk pos (fmt msg) in
  let report' ~pos msg = report ~pos msg [] None in
  let msg msg = Some (fmt msg) and no_msg = None in
  let annotation pos msg = (pos, Some (fmt msg)) in

  match message with
  (* Generic errors *)
  | Unexpected_character { position; character } ->
      report' ~pos:(lex_pos1 position) @@ fun f ->
      f "Unexpected character \"%s\"" (String.escaped character)
  | Unterminated_string { position; _ } ->
      report' ~pos:(lex_pos1 position) @@ fun f -> f "Unterminated string"
  | Unexpected_token { token; message } ->
      report' ~pos:(posd token) @@ fun f -> f "Unexpected %a: %s" pp_token token message
  (* Detailed errors *)
  | Table_key_equals token ->
      report ~pos:(posd token)
        (fun f -> f "Unexpected %a in expression." pp_code_s "=")
        []
        (msg (fun f ->
             f "Tip: Wrap the preceding expression in %a and %a to use it as a table key." pp_code_s
               "[" pp_code_s "]"))
  | Use_double_equals token ->
      report ~pos:(posd token)
        (fun f -> f "Unexpected %a in expression." pp_code_s "=")
        []
        (msg (fun f ->
             f "Tip: Replace this with %a to check if two values are equal." pp_code_s "=="))
  | Unclosed_brackets { open_; token } ->
      report ~pos:(posd token)
        (fun f -> f "Unexpected %a. Are you missing a closing bracket?" pp_token token)
        [ annotation (posd open_) (fun f -> f "Brackets were opened here.");
          annotation (posd token) (fun f -> f "Unexpected %a here." pp_token token)
        ]
        no_msg
  | Missing_table_comma { comma_pos; token } ->
      report ~pos:(posd token)
        (fun f -> f "Unexpected %a in table." pp_token token)
        [ (posd token, None);
          annotation (lex_pos1 comma_pos) (fun f -> f "Are you missing a comma here?")
        ]
        no_msg
  | Trailing_call_comma { comma; token } ->
      report ~pos:(posd token)
        (fun f -> f "Unexpected %a in function call." pp_token token)
        [ (posd token, None);
          annotation (posd comma) (fun f -> f "Tip: Try removing this %a." pp_code_s ",")
        ]
        no_msg
  | Local_function_dot { local; token } ->
      report ~pos:(posd token)
        (fun f -> f "Cannot use %a with a table key." pp_code_s "local function")
        [ annotation (posd token) (fun f -> f "%a appears here." pp_code_s ".");
          annotation (posd local) (fun f ->
              f "Tip: Try removing this %a keyword." pp_code_s "local")
        ]
        no_msg
  | Standalone_name token ->
      report ~pos:(posd token)
        (fun f -> f "Unexpected %a after name." pp_token token)
        []
        (msg (fun f -> f "Did you mean to assign this or call it as a function?"))
  | Standalone_names token ->
      report ~pos:(posd token)
        (fun f -> f "Unexpected %a after list of name." pp_token token)
        []
        (msg (fun f -> f "Did you mean to assign this?"))
  | Standalone_name_call pos ->
      report ~pos:(lex_pos1 pos)
        (fun f -> f "Unexpected symbol after name.")
        [ annotation (lex_pos1 pos) (fun f -> f "Expected something before the end of the line.") ]
        (msg (fun f -> f "Tip: Use %a to call with no arguments." pp_code_s "()"))
  | Expected_then { if_; pos } ->
      report ~pos:(span pos)
        (fun f -> f "Expected %a after if condition." pp_code_s "then")
        [ annotation (posd if_) (fun f -> f "If statement started here.");
          annotation (span pos) (fun f -> f "Expected %a before here." pp_code_s "then")
        ]
        no_msg
  | Expected_end { token; start } ->
      report ~pos:(posd token)
        (fun f ->
          f "Unexpected %a. Expected %a or another statement." pp_token token pp_code_s "end")
        [ annotation (span start) (fun f -> f "Block started here.");
          annotation (posd token) (fun f -> f "Expected end of block here.")
        ]
        no_msg
  | Unexpected_end token ->
      report ~pos:(posd token)
        (fun f -> f "Unexpected %a." pp_code_s "end")
        []
        (msg (fun f ->
             f
               "Your program contains more %as than needed. Check each block (%a, %a, %a, ...) \
                only has one %a"
               pp_code_s "end" pp_code_s "if" pp_code_s "for" pp_code_s "function" pp_code_s "end"))
  | Unfinished_label { start; token } ->
      report ~pos:(posd token)
        (fun f -> f "Unexpected %a." pp_token token)
        [ annotation (posd start) (fun f -> f "Label was started here.");
          annotation (posd token) (fun f -> f "Tip: Try adding %a here." pp_code_s "::")
        ]
        no_msg
  (* Boring fallback errors *)
  | Expected_statement token ->
      report' ~pos:(posd token) @@ fun f -> f "Unexpected %a. Expected a statement." pp_token token
  | Expected_function_args token ->
      report' ~pos:(posd token) @@ fun f ->
      f "Unexpected %a. Expected %a to start function arguments." pp_token token pp_code_s "("
  | Expected_expression token ->
      report' ~pos:(posd token) @@ fun f ->
      f "Unexpected %a. Expected an expression." pp_token token
  | Expected_var token ->
      report' ~pos:(posd token) @@ fun f ->
      f "Unexpected %a. Expected a variable name." pp_token token
