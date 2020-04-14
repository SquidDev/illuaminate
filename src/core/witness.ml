open Syntax

type 'a t =
  | Args : args t
  | BinOp : BinOp.t Node.t t
  | Call : call t
  | CallArgs : call_args t
  | Expr : expr t
  | FunctionName : function_name t
  | Name : name t
  | Program : program t
  | Stmt : stmt t
  | TableItem : table_item t
  | Token : token t
  | Var : var t

let name (type a) : a t -> string = function
  | Args -> "Args"
  | BinOp -> "Binop"
  | Call -> "Call"
  | CallArgs -> "CallArgs"
  | Expr -> "Expr"
  | FunctionName -> "FunctioName"
  | Name -> "Name"
  | Program -> "Program"
  | Stmt -> "Stmt"
  | TableItem -> "TableItem"
  | Token -> "Token"
  | Var -> "Var"

let first (type a) : a t -> (a, token) Lens.lens' = function
  | Args -> First.args
  | BinOp -> Node.lens_embed BinOp.token
  | Call -> First.call
  | CallArgs -> First.call_args
  | Expr -> First.expr
  | FunctionName -> First.function_name
  | Name -> First.name
  | Program -> First.program
  | Stmt -> First.stmt
  | TableItem -> First.table_item
  | Token -> Lens.Lenses.id
  | Var -> First.var

let last (type a) : a t -> (a, token) Lens.lens' = function
  | Args -> Last.args
  | BinOp -> Node.lens_embed BinOp.token
  | Call -> Last.call
  | CallArgs -> Last.call_args
  | Expr -> Last.expr
  | FunctionName -> Last.function_name
  | Name -> Last.name
  | Program -> Last.program
  | Stmt -> Last.stmt
  | TableItem -> Last.table_item
  | Token -> Lens.Lenses.id
  | Var -> Last.var

let span (type a) : a t -> a -> Span.t = function
  | Args -> Spanned.args
  (* While we could just use first/last, it's a nice micro-optimisation to avoid allocating the
     extra lens. *)
  | BinOp -> Node.span
  | Call -> Spanned.call
  | CallArgs -> Spanned.call_args
  | Expr -> Spanned.expr
  | FunctionName -> Spanned.function_name
  | Name -> Spanned.name
  | Program -> Spanned.program
  | Stmt -> Spanned.stmt
  | TableItem -> Spanned.table_item
  | Token -> Node.span
  | Var -> Spanned.var

let emit (type a) : a t -> Format.formatter -> a -> unit = function
  | Args -> Emit.args
  | BinOp -> Emit.node ~kind:OperatorKeyword BinOp.pp
  | Call -> Emit.call
  | CallArgs -> Emit.call_args
  | Expr -> Emit.expr
  | FunctionName -> Emit.function_name
  | Name -> Emit.name
  | Program -> Emit.program
  | Stmt -> Emit.stmt
  | TableItem -> Emit.table_item
  | Token -> Emit.token ~kind:Identifier
  | Var -> Emit.var
