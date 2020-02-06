open Syntax

(** The kind of token we're emitting. *)
type token_kind =
  | Keyword
  | LiteralKeyword  (** Keywords which act as literals. *)
  | OperatorKeyword  (** Keywords which act as operators. *)
  | Symbol
  | Identifier
  | String
  | Number
  | Comment

type Format.stag += Token of token_kind

let tprintf kind out fmt =
  Format.pp_open_stag out (Token kind);
  Format.kfprintf (fun x -> Format.pp_close_stag x ()) out fmt

let twith kind f out x =
  Format.pp_open_stag out (Token kind);
  f out x;
  Format.pp_close_stag out ()

let trivial out = function
  | Node.LineComment x -> tprintf Comment out "--%s" x
  | BlockComment (n, x) ->
      let eq = String.make n '=' in
      tprintf Comment out "--[%s[%s]%s]" eq x eq
  | Whitespace x -> Format.fprintf out "%s" x

let trivial_span out { Span.value; _ } = trivial out value

let node ~kind body out = function
  | Node.SimpleNode { contents } -> twith kind body out contents
  | Node.Node { leading_trivia; contents; trailing_trivia; _ } ->
      List.iter (trivial_span out) leading_trivia;
      twith kind body out contents;
      List.iter (trivial_span out) trailing_trivia

let token ~kind = node ~kind Token.pp

let rec list1 body out = function
  | SepList1.Mono x -> body out x
  | SepList1.Cons1 (x, t, xs) -> body out x; token ~kind:Symbol out t; list1 body out xs

let list0 body out = function
  | None -> ()
  | Some xs -> list1 body out xs

let idnt = node ~kind:Identifier Format.pp_print_string

let var out (Var x) = node ~kind:Identifier Format.pp_print_string out x

let literal ~kind out { lit_node; _ } = node ~kind Format.pp_print_string out lit_node

let rec do_stmt out { do_do; do_body; do_end } =
  token ~kind:Keyword out do_do; block out do_body; token ~kind:Keyword out do_end

and assign_stmt out { assign_vars; assign_eq; assign_vals } =
  list1 name out assign_vars; token ~kind:Symbol out assign_eq; list1 expr out assign_vals

and while_stmt out { while_while; while_test; while_do; while_body; while_end } =
  token ~kind:Keyword out while_while;
  expr out while_test;
  token ~kind:Keyword out while_do;
  block out while_body;
  token ~kind:Keyword out while_end

and repeat_stmt out { repeat_repeat; repeat_body; repeat_until; repeat_test } =
  token ~kind:Keyword out repeat_repeat;
  block out repeat_body;
  token ~kind:Keyword out repeat_until;
  expr out repeat_test

and for_num_stmt out
    { forn_for;
      forn_var;
      forn_eq;
      forn_start;
      forn_comma;
      forn_limit;
      forn_step;
      forn_do;
      forn_body;
      forn_end
    } =
  token ~kind:Keyword out forn_for;
  var out forn_var;
  token ~kind:Symbol out forn_eq;
  expr out forn_start;
  token ~kind:Symbol out forn_comma;
  expr out forn_limit;
  Option.iter (fun (t, e) -> token ~kind:Symbol out t; expr out e) forn_step;
  token ~kind:Keyword out forn_do;
  block out forn_body;
  token ~kind:Keyword out forn_end

and for_in_stmt out { forp_for; forp_vars; forp_in; forp_iter; forp_do; forp_body; forp_end } =
  token ~kind:Keyword out forp_for;
  list1 var out forp_vars;
  token ~kind:Keyword out forp_in;
  list1 expr out forp_iter;
  token ~kind:Keyword out forp_do;
  block out forp_body;
  token ~kind:Keyword out forp_end

and local_stmt out { local_local; local_vars; local_vals } =
  token ~kind:Keyword out local_local;
  list1 var out local_vars;
  Option.iter (fun (t, e) -> token ~kind:Symbol out t; list1 expr out e) local_vals

and local_function_stmt out
    { localf_local; localf_function; localf_var; localf_args; localf_body; localf_end } =
  token ~kind:Keyword out localf_local;
  token ~kind:Keyword out localf_function;
  var out localf_var;
  args out localf_args;
  block out localf_body;
  token ~kind:Keyword out localf_end

and function_stmt out { assignf_function; assignf_name; assignf_args; assignf_body; assignf_end } =
  token ~kind:Keyword out assignf_function;
  function_name out assignf_name;
  args out assignf_args;
  block out assignf_body;
  token ~kind:Keyword out assignf_end

and function_name out = function
  | FVar a -> var out a
  | FDot { tbl; dot; field } ->
      function_name out tbl;
      token ~kind:Symbol out dot;
      node ~kind:Symbol Format.pp_print_string out field
  | FSelf { tbl; colon; meth } ->
      function_name out tbl;
      token ~kind:Symbol out colon;
      node ~kind:Symbol Format.pp_print_string out meth

and return_stmt out { return_return; return_vals } =
  token ~kind:Keyword out return_return;
  list0 expr out return_vals

and if_stmt out { if_if; if_elseif; if_else; if_end } =
  if_clause out if_if;
  List.iter (if_clause out) if_elseif;
  Option.iter (fun (t, b) -> token ~kind:Keyword out t; block out b) if_else;
  token ~kind:Keyword out if_end

and if_clause out { clause_if; clause_test; clause_then; clause_body } =
  token ~kind:Keyword out clause_if;
  expr out clause_test;
  token ~kind:Keyword out clause_then;
  block out clause_body

and stmt out = function
  | Do a -> do_stmt out a
  | Assign a -> assign_stmt out a
  | While a -> while_stmt out a
  | Repeat a -> repeat_stmt out a
  | ForNum a -> for_num_stmt out a
  | ForIn a -> for_in_stmt out a
  | Local a -> local_stmt out a
  | LocalFunction a -> local_function_stmt out a
  | AssignFunction a -> function_stmt out a
  | Return a -> return_stmt out a
  | If a -> if_stmt out a
  | Break a -> token ~kind:Keyword out a
  | SCall a -> call out a
  | Semicolon a -> token ~kind:Symbol out a

and block out x = List.iter (stmt out) x

and name out = function
  | NVar a -> var out a
  | NDot { tbl; dot; key } ->
      expr out tbl;
      token ~kind:Symbol out dot;
      node ~kind:Symbol Format.pp_print_string out key
  | NLookup { tbl; open_k; key; close_k } ->
      expr out tbl; token ~kind:Symbol out open_k; expr out key; token ~kind:Symbol out close_k

and fun_expr out { fun_function; fun_args; fun_body; fun_end } =
  token ~kind:Keyword out fun_function;
  args out fun_args;
  block out fun_body;
  token ~kind:Keyword out fun_end

and table out { table_open; table_body; table_close } =
  token ~kind:Symbol out table_open;
  List.iter
    (fun (x, t) ->
      table_item out x;
      Option.iter (token ~kind:Symbol out) t)
    table_body;
  token ~kind:Symbol out table_close

and table_item out = function
  | Array a -> expr out a
  | RawPair { ident; eq; value } -> idnt out ident; token ~kind:Symbol out eq; expr out value
  | ExprPair { open_k; key; close_k; eq; value } ->
      token ~kind:Symbol out open_k;
      expr out key;
      token ~kind:Symbol out close_k;
      token ~kind:Symbol out eq;
      expr out value

and unop_expr out { unop_op; unop_rhs } =
  let kind =
    match Node.contents.get unop_op with
    | OpNot -> OperatorKeyword
    | _ -> Symbol
  in
  node ~kind UnOp.pp out unop_op; expr out unop_rhs

and binop_expr out { binop_lhs; binop_op; binop_rhs } =
  let kind =
    match Node.contents.get binop_op with
    | OpAnd | OpOr -> OperatorKeyword
    | _ -> Symbol
  in
  expr out binop_lhs; node ~kind BinOp.pp out binop_op; expr out binop_rhs

and paren_expr out { paren_open; paren_expr; paren_close } =
  token ~kind:Symbol out paren_open;
  expr out paren_expr;
  token ~kind:Symbol out paren_close

and expr out = function
  | Ref a -> name out a
  | ECall a -> call out a
  | Dots a -> token ~kind:Symbol out a
  | Nil a -> token ~kind:LiteralKeyword out a
  | True a -> token ~kind:LiteralKeyword out a
  | False a -> token ~kind:LiteralKeyword out a
  | Number a -> literal ~kind:Number out a
  | Int a -> literal ~kind:Number out a
  | MalformedNumber a -> node ~kind:Number Format.pp_print_string out a
  | String a -> literal ~kind:String out a
  | Fun a -> fun_expr out a
  | Table a -> table out a
  | UnOp a -> unop_expr out a
  | BinOp a -> binop_expr out a
  | Parens a -> paren_expr out a

and call out = function
  | Call { fn; args } -> expr out fn; call_args out args
  | Invoke { obj; colon; meth; args } ->
      expr out obj;
      token ~kind:Symbol out colon;
      node ~kind:Symbol Format.pp_print_string out meth;
      call_args out args

and call_args out = function
  | CallArgs { open_a; args; close_a } ->
      token ~kind:Symbol out open_a; list0 expr out args; token ~kind:Symbol out close_a
  | CallTable a -> table out a
  | CallString a -> literal ~kind:String out a

and args out { args_open; args_args; args_close } =
  token ~kind:Symbol out args_open; list0 arg out args_args; token ~kind:Symbol out args_close

and arg out = function
  | NamedArg a -> var out a
  | DotArg a -> token ~kind:Symbol out a

let program out program =
  block out program.Syntax.program;
  node ~kind:Symbol (fun _ _ -> ()) out program.eof

let repl_exprs out repl =
  list1 expr out repl.repl_exprs;
  node ~kind:Symbol (fun _ _ -> ()) out repl.repl_eof
