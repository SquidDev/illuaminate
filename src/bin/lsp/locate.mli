open IlluaminateCore.Syntax

type node =
  | Var of var
  | Arg of arg
  | Name of name
  | FunctionName of function_name
  | Expr of expr
  | TableItem of table_item
  | Stmt of stmt
  | Program of program

val pp_node : Format.formatter -> node -> unit

val pp_node_short : Format.formatter -> node -> unit

(** Attempt to locate the node the cursor is currently within.*)
val locate : Lsp.Types.Position.t -> program -> node
