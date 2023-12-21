open struct
  module E = IlluaminateCore.Error
end

type t =
  | Unexpected_character of string  (** An unexpected character was used.*)
  | Unterminated_string  (**A string which ends without a closing quote.*)
  | Unexpected_token of
      { token : Grammar.token;  (** The unexpected token in question. *)
        message : string  (** The parser error, daken from [messages.txt]. *)
      }  (** A fallback error when we can't produce anything more useful.*)
  | Table_key_equals of Grammar.token  (** [=] was used after an expression inside a table. *)
  | Use_double_equals of Grammar.token  (** [=] was used in an expression context. *)
  | Unclosed_brackets of
      { open_ : IlluaminateCore.Syntax.token;  (** The opening parenthesis. *)
        token : Grammar.token  (** The current token. *)
      }  (** A parenthesised expression was started but not closed. *)
  | Missing_table_comma of
      { token : Grammar.token;  (** The current token. *)
        comma_pos : IlluaminateCore.Span.t  (** The position where we expected the comma. *)
      }  (** A missing comma between tokens. *)
  | Trailing_call_comma of
      { comma : IlluaminateCore.Syntax.token;  (** The [,] token*)
        token : Grammar.token  (** The closing [)]. *)
      }  (** A superflous [,] in a function call. *)
  | Local_function_dot of
      { local : IlluaminateCore.Syntax.token;  (** The [local] token *)
        token : Grammar.token  (** The invalid [.] token. *)
      }  (** [local function] was used with a table identifier.*)
  | Standalone_name of Grammar.token  (** A statement of the form [x.y z]*)
  | Standalone_name_call of IlluaminateCore.Span.t
      (** A statement of the form [x.y]. This is similar to {!Standalone_name}, but when the next
          token is on another line. *)
  | Expected_then of
      { if_ : IlluaminateCore.Syntax.token;  (** The [if] or [elseif] token. *)
        pos : IlluaminateCore.Span.t  (** The current position. *)
      }  (** [then] was expected. *)
  | Expected_end of
      { start : IlluaminateCore.Span.t;  (** The start position of this block. *)
        token : Grammar.token  (** The current token. *)
      }  (** [end] was expected. *)
  | Unexpected_end of Grammar.token  (**An unexpected [end] in a statement. *)
  | Unfinished_label of
      { start : IlluaminateCore.Syntax.token;  (** The opening [::]. *)
        token : Grammar.token  (** The current token. *)
      }  (** A label declaration was started but not closed. *)
  | Expected_statement of Grammar.token
      (** A fallback error when we expected a statement but received another token. *)
  | Expected_function_args of Grammar.token  (** Expected `(` to open our function arguments. *)
  | Expected_expression of Grammar.token
      (** A fallback error when we expected an expression but received another token. *)
  | Expected_var of Grammar.token
      (** A fallback error when we expected a variable but received another token. *)

let tag = E.Tag.make ~attr:[] ~level:E.Critical "parse:syntax-error"

let report errs pos err =
  let pp_code pp = Fmt.styled `Underline (fun o -> Fmt.fmt "`%a`" o pp) in
  let pp_code_s = pp_code Format.pp_print_string in
  let pp_token out : Grammar.token -> unit = function
    | EOF _ -> Format.pp_print_string out "end of file"
    | IDENT _ -> Format.pp_print_string out "identifier"
    | STRING _ -> Format.pp_print_string out "string"
    | NUMBER _ -> Format.pp_print_string out "number"
    | token -> pp_code IlluaminateCore.Token.pp out (Token.get_token token)
  in
  let fmt msg out () = msg (Fmt.pf out) in
  let report msg = E.report_detailed errs tag pos (fmt msg) in
  let report' msg = report msg [] in
  let msg msg = E.Error.Message (fmt msg) in
  let annotation pos msg = E.Error.Annotation (pos, Some (fmt msg)) in
  match err with
  (* Generic errors *)
  | Unexpected_character c -> report' @@ fun f -> f "Unexpected character \"%s\"" (String.escaped c)
  | Unterminated_string -> report' @@ fun f -> f "Unterminated string"
  | Unexpected_token { token; message } ->
      report' @@ fun f -> f "Unexpected %a: %s" pp_token token message
  (* Detailed errors *)
  | Table_key_equals _ ->
      report
        (fun f -> f "Unexpected %a in expression." pp_code_s "=")
        [ Annotation (pos, None);
          msg (fun f ->
              f "Tip: Wrap the preceding expression in %a and %a to use it as a table key."
                pp_code_s "[" pp_code_s "]")
        ]
  | Use_double_equals _ ->
      report
        (fun f -> f "Unexpected %a in expression." pp_code_s "=")
        [ Annotation (pos, None);
          msg (fun f ->
              f "Tip: Replace this with %a to check if two values are equal." pp_code_s "==")
        ]
  | Unclosed_brackets { open_; token } ->
      report
        (fun f -> f "Unexpected %a. Are you missing a closing bracket?" pp_token token)
        [ annotation (IlluaminateCore.Node.span open_) (fun f -> f "Brackets were opened here.");
          annotation (Token.get_span token) (fun f -> f "Unexpected %a here." pp_token token)
        ]
  | Missing_table_comma { comma_pos; token } ->
      report
        (fun f -> f "Unexpected %a in table." pp_token token)
        [ Annotation (Token.get_span token, None);
          annotation comma_pos (fun f -> f "Are you missing a comma here?")
        ]
  | Trailing_call_comma { comma; token } ->
      report
        (fun f -> f "Unexpected %a in function call." pp_token token)
        [ Annotation (Token.get_span token, None);
          annotation (IlluaminateCore.Node.span comma) (fun f ->
              f "Tip: Try removing this %a." pp_code_s ",")
        ]
  | Local_function_dot { local; token } ->
      report
        (fun f -> f "Cannot use %a with a table key." pp_code_s "local function")
        [ annotation (Token.get_span token) (fun f -> f "%a appears here." pp_code_s ".");
          annotation (IlluaminateCore.Node.span local) (fun f ->
              f "Tip: Try removing this %a keyword." pp_code_s "local")
        ]
  | Standalone_name token ->
      report
        (fun f -> f "Unexpected %a after name." pp_token token)
        [ Annotation (Token.get_span token |> IlluaminateCore.Span.start, None);
          msg (fun f -> f "Did you mean to assign this or call it as a function?")
        ]
  | Standalone_name_call pos ->
      report
        (fun f -> f "Unexpected symbol after name.")
        [ annotation pos (fun f -> f "Expected something before the end of the line.");
          msg (fun f -> f "Tip: Use %a to call with no arguments." pp_code_s "()")
        ]
  | Expected_then { if_; pos } ->
      report
        (fun f -> f "Expected %a after if condition." pp_code_s "then")
        [ annotation (IlluaminateCore.Node.span if_) (fun f -> f "If statement started here.");
          annotation pos (fun f -> f "Expected %a before here." pp_code_s "then")
        ]
  | Expected_end { token; start } ->
      report
        (fun f ->
          f "Unexpected %a. Expected %a or another statement." pp_token token pp_code_s "end")
        [ annotation start (fun f -> f "Block started here.");
          annotation (Token.get_span token) (fun f -> f "Expected end of block here.")
        ]
  | Unexpected_end _ ->
      report
        (fun f -> f "Unexpected %a." pp_code_s "end")
        [ Annotation (pos, None);
          msg (fun f ->
              f
                "Your program contains more %as than needed. Check each block (%a, %a, %a, ...) \
                 only has one %a"
                pp_code_s "end" pp_code_s "if" pp_code_s "for" pp_code_s "function" pp_code_s "end")
        ]
  | Unfinished_label { start; token } ->
      report
        (fun f -> f "Unexpected %a." pp_token token)
        [ annotation (IlluaminateCore.Node.span start) (fun f -> f "Label was started here.");
          annotation (Token.get_span token) (fun f -> f "Tip: Try adding %a here." pp_code_s "::")
        ]
  (* Boring fallback errors *)
  | Expected_statement token ->
      report' @@ fun f -> f "Unexpected %a. Expected a statement." pp_token token
  | Expected_function_args token ->
      report' @@ fun f ->
      f "Unexpected %a. Expected %a to start function arguments." pp_token token pp_code_s "("
  | Expected_expression token ->
      report' @@ fun f -> f "Unexpected %a. Expected an expression." pp_token token
  | Expected_var token ->
      report' @@ fun f -> f "Unexpected %a. Expected a variable name." pp_token token
