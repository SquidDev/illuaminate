open Syntax

type 'a t =
  | BinOp : BinOp.t Node.t t
  | Call : call t
  | Expr : expr t
  | Name : name t
  | Program : program t
  | Stmt : stmt t
  | Token : token t
  | Var : var t

let first (type a) : a t -> (a, token) Lens.lens' = function
  | BinOp -> Node.lens_embed BinOp.token
  | Call -> First.call
  | Expr -> First.expr
  | Name -> First.name
  | Program -> First.program
  | Stmt -> First.stmt
  | Token -> Lens.Lenses.id
  | Var -> First.var

let last (type a) : a t -> (a, token) Lens.lens' = function
  | BinOp -> Node.lens_embed BinOp.token
  | Call -> Last.call
  | Expr -> Last.expr
  | Name -> Last.name
  | Program -> Last.program
  | Stmt -> Last.stmt
  | Token -> Lens.Lenses.id
  | Var -> Last.var

let span (type a) : a t -> a -> Span.t = function
  (* While we could just use first/last, it's a nice micro-optimisation to avoid allocating the
     extra lens. *)
  | BinOp -> Node.span
  | Call -> Spanned.call
  | Expr -> Spanned.expr
  | Name -> Spanned.name
  | Program -> Spanned.program
  | Stmt -> Spanned.stmt
  | Token -> Node.span
  | Var -> Spanned.var

let emit (type a) : a t -> Format.formatter -> a -> unit = function
  | BinOp -> Emit.node ~kind:OperatorKeyword BinOp.pp
  | Call -> Emit.call
  | Expr -> Emit.expr
  | Name -> Emit.name
  | Program -> Emit.program
  | Stmt -> Emit.stmt
  | Token -> Emit.token ~kind:Identifier
  | Var -> Emit.var
