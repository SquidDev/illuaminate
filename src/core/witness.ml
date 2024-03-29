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
  | File : File.t t

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
  | File -> "File"

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
  | File -> File.span

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
  | File -> File.emit
