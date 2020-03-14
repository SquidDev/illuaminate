open IlluaminateCore
open Syntax
open Lsp_convert

type node =
  | Var of var
  | DotArg of token
  | Name of name
  | FunctionName of function_name
  | Expr of expr
  | TableItem of table_item
  | Stmt of stmt
  | Program of program

let pp_node out : node -> unit = function
  | Var _ -> Format.fprintf out "Var"
  | DotArg _ -> Format.fprintf out "DotArg"
  | Name _ -> Format.fprintf out "Name"
  | FunctionName _ -> Format.fprintf out "FunctionName"
  | TableItem _ -> Format.fprintf out "TableItem"
  | Expr e -> Format.fprintf out "Expr %a" Emit.expr e
  | Stmt s -> Format.fprintf out "Stmt %a" Emit.stmt s
  | Program p -> Format.fprintf out "Program %a" Emit.program p

let node_kind : node -> string = function
  | Var _ -> "Var"
  | DotArg _ -> "DotArg"
  | Name _ -> "Name"
  | FunctionName _ -> "FunctionName"
  | TableItem _ -> "TableItem"
  | Expr _ -> "Expr"
  | Stmt _ -> "Stmt"
  | Program _ -> "Program"

let pp_node_short out f = Format.fprintf out "%s" (node_kind f)

let ( <|> ) x d = Option.value ~default:d x [@@inline]

(** Use let-syntax to provide a way to write lazy option chains without noisy syntax. *)
let ( let+ ) x d =
  match x with
  | None -> d ()
  | Some x -> x
  [@@inline]

let ( let* ) x d =
  match x with
  | None -> d ()
  | Some _ -> x
  [@@inline]

let rec seplist1 span f pos = function
  | SepList1.Mono x -> if Pos.contains pos (span x) then Some (f pos x) else None
  | SepList1.Cons1 (x, _, xs) ->
      if Pos.contains pos (span x) then Some (f pos x) else seplist1 span f pos xs

let seplist0 span f pos = function
  | None -> None
  | Some x -> seplist1 span f pos x

let rec list span f pos = function
  | [] -> None
  | x :: xs -> if Pos.contains pos (span x) then Some (f pos x) else list span f pos xs

let rec block p = list Spanned.stmt stmt p

and stmt pos stmt : node =
  match stmt with
  | Do { do_body; _ } -> block pos do_body <|> Stmt stmt
  | Assign { assign_vars; assign_vals; _ } ->
      let+ () = seplist1 Spanned.name name pos assign_vars in
      seplist1 Spanned.expr expr pos assign_vals <|> Stmt stmt
  | While { while_test; while_body; _ } ->
      let+ () = expr_opt pos while_test in
      block pos while_body <|> Stmt stmt
  | Repeat { repeat_body; repeat_test; _ } ->
      let+ () = block pos repeat_body in
      expr_opt pos repeat_test <|> Stmt stmt
  | ForNum { forn_var; forn_start; forn_limit; forn_step; forn_body; _ } ->
      let+ () = var_opt pos forn_var in
      let+ () = expr_opt pos forn_start in
      let+ () = Option.bind forn_step (fun (_, e) -> expr_opt pos e) in
      let+ () = expr_opt pos forn_limit in
      block pos forn_body <|> Stmt stmt
  | ForIn { forp_vars; forp_iter; forp_body; _ } ->
      let+ () = seplist1 Spanned.var var pos forp_vars in
      let+ () = seplist1 Spanned.expr expr pos forp_iter in
      block pos forp_body <|> Stmt stmt
  | Local { local_vars; local_vals; _ } ->
      let+ () = seplist1 Spanned.var var pos local_vars in
      Option.bind local_vals (fun (_, xs) -> seplist1 Spanned.expr expr pos xs) <|> Stmt stmt
  | LocalFunction { localf_var; localf_args; localf_body; _ } ->
      let+ () = var_opt pos localf_var in
      let+ () = args pos localf_args in
      block pos localf_body <|> Stmt stmt
  | AssignFunction { assignf_name; assignf_args; assignf_body; _ } ->
      if Pos.contains pos (Spanned.function_name assignf_name) then function_name pos assignf_name
      else
        let+ () = args pos assignf_args in
        block pos assignf_body <|> Stmt stmt
  | Return { return_vals; _ } -> seplist0 Spanned.expr expr pos return_vals <|> Stmt stmt
  | If { if_if; if_elseif; if_else; if_end; _ } ->
      let last = Option.fold ~none:if_end ~some:(fun (t, _) -> t) if_else in
      (* Rather gross: we make an optimisation here, so that we check that the position is within
         the if/elseif/else/end pairs before checking each body. This should probably be done on all
         nodes TBH. *)
      let rec clauses = function
        | [] -> (
          match if_else with
          | Some (e, xs)
            when Pos.(span_start (Node.span e) <= pos && pos <= span_finish (Node.span if_end)) ->
              block pos xs <|> Stmt stmt
          | _ -> Stmt stmt )
        | x :: (y :: _ as xs) ->
            let+ () = if_clause pos x y.clause_if in
            clauses xs
        | [ x ] ->
            let+ () = if_clause pos x last in
            clauses []
      in
      let+ () =
        if_clause pos if_if
          ( match if_elseif with
          | [] -> last
          | x :: _ -> x.clause_if )
      in
      clauses if_elseif
  | SCall c -> call pos c <|> Stmt stmt
  | Semicolon _ | Break _ -> Stmt stmt

and if_clause pos { clause_if; clause_test; clause_body; _ } next : node option =
  if Pos.(span_start (Node.span clause_if) <= pos && pos <= span_finish (Node.span next)) then
    let* () = expr_opt pos clause_test in
    block pos clause_body
  else None

and name pos name : node =
  match name with
  | NVar v -> Var v
  | NDot { tbl; _ } -> expr_opt pos tbl <|> Name name
  | NLookup { tbl; key; _ } ->
      let+ () = expr_opt pos tbl in
      expr_opt pos key <|> Name name

and function_name pos name : node =
  match name with
  | FVar v -> Var v
  | FDot { tbl; _ } | FSelf { tbl; _ } ->
      if Pos.contains pos (Spanned.function_name tbl) then function_name pos tbl
      else FunctionName name

and var _ v = Var v

and var_opt pos var : node option =
  if Pos.contains pos (Spanned.var var) then Some (Var var) else None

and table_items p = list (fun (x, _) -> Spanned.table_item x) table_item p

and table_item pos (t, _) =
  match t with
  | Array e -> expr pos e
  | RawPair { value; _ } -> expr_opt pos value <|> TableItem t
  | ExprPair { key; value; _ } ->
      let+ () = expr_opt pos key in
      expr_opt pos value <|> TableItem t

and expr pos e : node =
  match e with
  | Ref n -> name pos n
  | Dots _ | Nil _ | True _ | False _ | Number _ | Int _ | MalformedNumber _ | String _ -> Expr e
  | ECall c -> call pos c <|> Expr e
  | Fun { fun_args; fun_body; _ } ->
      let+ () = args pos fun_args in
      block pos fun_body <|> Expr e
  | Table { table_body; _ } -> table_items pos table_body <|> Expr e
  | UnOp { unop_rhs; _ } -> expr_opt pos unop_rhs <|> Expr e
  | BinOp { binop_lhs; binop_rhs; _ } ->
      let+ () = expr_opt pos binop_lhs in
      expr_opt pos binop_rhs <|> Expr e
  | Parens { paren_expr; _ } -> expr_opt pos paren_expr <|> Expr e

and expr_opt pos e = if Pos.contains pos (Spanned.expr e) then Some (expr pos e) else None

and call pos : call -> node option = function
  | Call { fn; args } | Invoke { obj = fn; args; _ } ->
      let* () = expr_opt pos fn in
      call_args pos args

and call_args pos : call_args -> node option = function
  | CallTable { table_body; _ } -> table_items pos table_body
  | CallString _ -> None
  | CallArgs { args; _ } -> seplist0 Spanned.expr expr pos args

and args pos { args_args; _ } = seplist0 Spanned.arg arg pos args_args

and arg _ = function
  | NamedArg v -> Var v
  | DotArg v -> DotArg v

let locate pos program = block pos program.program <|> Program program
