open IlluaminateCore.Syntax

type node =
  | DotArg of token
  | Expr of expr
  | FunctionName of function_name
  | Name of name
  | Program of program
  | Stmt of stmt
  | TableItem of table_item
  | Var of var

val pp_node : Format.formatter -> node -> unit

val pp_node_short : Format.formatter -> node -> unit

(** Attempt to locate the node the cursor is currently within.*)
val locate : Lsp.Types.Position.t -> program -> node
