%token <IlluaminateCore.Syntax.token> BREAK DO ELSE ELSEIF END FALSE FOR FUNCTION IF IN LOCAL NIL
%token <IlluaminateCore.Syntax.token> REPEAT RETURN THEN TRUE UNTIL WHILE EOF

%token <IlluaminateCore.Syntax.token> COLON ":"
%token <IlluaminateCore.Syntax.token> COMMA ","
%token <IlluaminateCore.Syntax.token> DOT "."
%token <IlluaminateCore.Syntax.token> DOTS "..."
%token <IlluaminateCore.Syntax.token> EQUALS "="
%token <IlluaminateCore.Syntax.token> SEMICOLON ";"

%token <IlluaminateCore.Syntax.token> OPAREN "(" CPAREN ")"
%token <IlluaminateCore.Syntax.token> OBRACE "{" CBRACE "}"
%token <IlluaminateCore.Syntax.token> OSQUARE "[" CSQUARE "]"

%token <IlluaminateCore.Syntax.BinOp.t IlluaminateCore.Node.t> ADD "+" SUB "-" MUL "*" DIV "/" POW "^" MOD "%"
%token <IlluaminateCore.Syntax.BinOp.t IlluaminateCore.Node.t> CONCAT ".."
%token <IlluaminateCore.Syntax.BinOp.t IlluaminateCore.Node.t> EQ "==" NE "~=" LT "<" LE "<=" GT ">" GE ">="
%token <IlluaminateCore.Syntax.UnOp.t IlluaminateCore.Node.t> LEN "#"

%token <IlluaminateCore.Syntax.BinOp.t IlluaminateCore.Node.t> AND OR
%token <IlluaminateCore.Syntax.UnOp.t IlluaminateCore.Node.t> NOT

%token <string IlluaminateCore.Node.t> IDENT
%token <string IlluaminateCore.Syntax.literal> STRING
%token <int IlluaminateCore.Syntax.literal> INT
%token <float IlluaminateCore.Syntax.literal> NUMBER
%token <string IlluaminateCore.Node.t> MNUMBER

%left OR
%left AND
%left "<" ">" "<=" ">=" "~=" "=="
%right ".."
%left "+" "-"
%left "*" "/" "%"
%right NOT
%right "^"

%start <IlluaminateCore.Syntax.program> main

%type <IlluaminateCore.Syntax.name> name
%type <IlluaminateCore.Syntax.var> var
%type <IlluaminateCore.Syntax.expr> simple_expr atom expr expr_pow
%type <IlluaminateCore.Syntax.stmt> stmt

%on_error_reduce
  name
  var

  expr_pow

  stmts
  if_clause(ELSEIF)

(* Sadly we can't do [module M = ...] at the top, as Menhir doesn't load that. *)
%{ open IlluaminateCore.Syntax %}

%%

let main := program = stmts ; eof = EOF ; { { program; eof } }

let var := ~ = IDENT ; <Var>

let arg :=
  | ~ = var ; <NamedArg>
  | ~ = "..." ; <DotArg>

let args :=
  | args_open = "(" ; args_args = sep_list0(",", arg) ; args_close = ")"
  ; { { args_open; args_args; args_close } }

let name :=
  | ~ = var
  ; <NVar>
  | tbl = simple_expr ; dot = "." ; key = IDENT
  ; { NDot { tbl; dot; key } }
  | tbl = simple_expr ; open_k = "[" ; key = expr ; close_k = "]"
  ; { NLookup { tbl; open_k; key; close_k } }

let simple_expr :=
  | ~ = name ; <Ref>
  | paren_open = "(" ; paren_expr = expr ; paren_close = ")"
  ; { Parens { paren_open; paren_expr; paren_close } }
  | ~ = call ; <ECall>

let call :=
  | fn = simple_expr ; args = call_args
  ; { Call { fn; args } }
  | fn = simple_expr ; colon = ":" ; meth = IDENT ; args = call_args
  ; { Invoke { fn; colon; meth; args } }

let call_args :=
  | open_a = "(" ; args = sep_list0(",", expr) ; close_a = ")"
  ; { CallArgs { open_a; args; close_a } }
  | ~ = STRING ; <CallString>
  | ~ = table  ; <CallTable>

(* Expressions *)
let atom :=
  | simple_expr
  | ~ = table   ; <Table>
  | ~ = NIL     ; <Nil>
  | ~ = TRUE    ; <True>
  | ~ = FALSE   ; <False>
  | ~ = "..."   ; <Dots>
  | ~ = INT     ; <Int>
  | ~ = NUMBER  ; <Number>
  | ~ = MNUMBER ; <MalformedNumber>
  | ~ = STRING  ; <String>
  | fun_function = FUNCTION ; fun_args = args ; fun_body = stmts ; fun_end = END
  ; { Fun { fun_function; fun_args; fun_body; fun_end } }

let expr :=
  | expr_pow

  | binop_lhs = expr ; binop_op = AND ; binop_rhs = expr ; { BinOp { binop_lhs; binop_op; binop_rhs } }
  | binop_lhs = expr ; binop_op = OR  ; binop_rhs = expr ; { BinOp { binop_lhs; binop_op; binop_rhs } }


  | binop_lhs = expr ; binop_op = "+" ; binop_rhs = expr ; { BinOp { binop_lhs; binop_op; binop_rhs } }
  | binop_lhs = expr ; binop_op = "-" ; binop_rhs = expr ; { BinOp { binop_lhs; binop_op; binop_rhs } }
  | binop_lhs = expr ; binop_op = "*" ; binop_rhs = expr ; { BinOp { binop_lhs; binop_op; binop_rhs } }
  | binop_lhs = expr ; binop_op = "/" ; binop_rhs = expr ; { BinOp { binop_lhs; binop_op; binop_rhs } }
  | binop_lhs = expr ; binop_op = "%" ; binop_rhs = expr ; { BinOp { binop_lhs; binop_op; binop_rhs } }

  | binop_lhs = expr ; binop_op = ".." ; binop_rhs = expr ; { BinOp { binop_lhs; binop_op; binop_rhs } }

  | binop_lhs = expr ; binop_op = "==" ; binop_rhs = expr ; { BinOp { binop_lhs; binop_op; binop_rhs } }
  | binop_lhs = expr ; binop_op = "~=" ; binop_rhs = expr ; { BinOp { binop_lhs; binop_op; binop_rhs } }
  | binop_lhs = expr ; binop_op = "<"  ; binop_rhs = expr ; { BinOp { binop_lhs; binop_op; binop_rhs } }
  | binop_lhs = expr ; binop_op = "<=" ; binop_rhs = expr ; { BinOp { binop_lhs; binop_op; binop_rhs } }
  | binop_lhs = expr ; binop_op = ">"  ; binop_rhs = expr ; { BinOp { binop_lhs; binop_op; binop_rhs } }
  | binop_lhs = expr ; binop_op = ">=" ; binop_rhs = expr ; { BinOp { binop_lhs; binop_op; binop_rhs } }

let expr_pow :=
  | atom
  | binop_lhs = expr_pow ; binop_op = "^" ; binop_rhs = expr_pow
  ; { BinOp { binop_lhs; binop_op; binop_rhs } }

  | unop_op = "-" ; unop_rhs = expr_pow
  ; { UnOp { unop_op = IlluaminateCore.Node.with_contents UnOp.OpNeg unop_op; unop_rhs } } %prec NOT
  | unop_op = "#" ; unop_rhs = expr_pow ; { UnOp { unop_op; unop_rhs } } %prec NOT
  | unop_op = NOT ; unop_rhs = expr_pow ; { UnOp { unop_op; unop_rhs } } %prec NOT

(* Tables *)
let table :=
  | table_open = "{" ; table_body = table_body ; table_close = "}"
  ; { { table_open; table_body; table_close } }

let table_sep := ";" | ","

let table_body :=
  | { [] }
  | x = table_entry ; { [x, None] }
  | x = table_entry ; s = table_sep ; xs = table_body ; { (x, Some s) :: xs }

let table_entry :=
  | ~ = expr ;  <Array>
  | ident = IDENT ; eq = "=" ; value = expr ; { RawPair { ident; eq; value } }
  | open_k = "[" ; key = expr ; close_k = "]" ; eq = "=" ; value = expr
  ; { ExprPair { open_k; key; close_k; eq; value } }

(* Statements *)

let stmts := list(stmt)

let stmt :=
  | ~ = ";" ; <Semicolon>
  | ~ = call ; <SCall>
  | ~ = BREAK ; <Break>

  | do_do = DO ; do_body = stmts ; do_end = END
  ; { Do { do_do; do_body; do_end } }

  | assign_vars = sep_list1(",", name) ; assign_eq = "=" ; assign_vals = sep_list1(",", expr)
  ; { Assign { assign_vars; assign_eq; assign_vals } }

  | while_while = WHILE ; while_test = expr ; while_do = DO ; while_body = stmts ; while_end = END
  ; { While { while_while; while_test; while_do; while_body; while_end } }

  | repeat_repeat = REPEAT ; repeat_body = stmts ; repeat_until = UNTIL ; repeat_test = expr
  ; { Repeat { repeat_repeat; repeat_body; repeat_until; repeat_test } }

  | if_if = if_clause(IF) ; if_elseif = list(if_clause(ELSEIF))
  ; if_else = option(x = ELSE ; y = stmts ; { (x, y) }) ; if_end = END
  ; { If { if_if; if_elseif; if_else; if_end } }

  | forn_for = FOR ; forn_var = var ; forn_eq = "=" ; forn_start = expr ; forn_comma = ","; forn_limit = expr
  ; forn_step = option (x = "," ; y = expr ; { (x, y) }) ; forn_do = DO ; forn_body = stmts ; forn_end = END
  ; { ForNum { forn_for; forn_var; forn_eq; forn_start; forn_comma; forn_limit; forn_step; forn_do; forn_body; forn_end } }

  | forp_for = FOR ; forp_vars = sep_list1(",", var) ; forp_in = IN ; forp_iter = sep_list1(",", expr)
  ; forp_do = DO ; forp_body = stmts ; forp_end = END
  ; { ForIn { forp_for; forp_vars; forp_in; forp_iter; forp_do; forp_body; forp_end } }

  | local_local = LOCAL ; local_vars  = sep_list1(",", var)
  ; local_vals = option(x = "=" ; y = sep_list1(",", expr) ; { (x, y) })
  ; { Local { local_local; local_vars; local_vals } }

  | localf_local = LOCAL ; localf_function = FUNCTION ; localf_var = var ; localf_args = args
  ; localf_body = stmts ; localf_end = END
  ; { LocalFunction { localf_local; localf_function; localf_var; localf_args; localf_body; localf_end } }

  | assignf_function = FUNCTION ; assignf_name = function_name; assignf_args = args
  ; assignf_body = stmts ; assignf_end = END
  ; { AssignFunction { assignf_function; assignf_name; assignf_args; assignf_body; assignf_end } }

  | return_return = RETURN ; return_vals = sep_list0(",", expr)
  ; { Return { return_return; return_vals } }

let function_name :=
  | ~ = var ; <FVar>
  | tbl = function_name ; dot = "."   ; field = IDENT ; { FDot { tbl; dot; field } }
  | tbl = function_name ; colon = ":" ; meth = IDENT  ; { FSelf { tbl; colon; meth } }

let if_clause(t) :=
  | clause_if = t ; clause_test = expr ; clause_then = THEN ; clause_body = stmts ;
  { { clause_if; clause_test; clause_then; clause_body } }

let sep_list1(separator, X) :=
  | ~ = X ; <SepList1.Mono>
  | x = X; sep = separator; xs = sep_list1(separator, X)
  ; { Cons1 (x, sep, xs) }

let sep_list0(separator, X) :=
  | { None }
  | ~ = sep_list1(separator, X) ; <Some>
