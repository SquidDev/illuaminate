(** A "witness" provides a proof that a term is a {!Syntax} node of a specific type, such as an
    expression or statement.

    With such a witness, you are able to perform various operations (such as extracting the
    first/last node) without knowing the type of term at compile time. *)
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

(** Get the name of this witness. *)
val name : 'a t -> string

(** A lens to return the first token in the term, much like with {!First}. *)
val first : 'a t -> ('a, token) Illuaminate.Lens.lens'

(** A lens to return the last token in the term, much like with {!Last}. *)
val last : 'a t -> ('a, token) Illuaminate.Lens.lens'

(** Get the span of this term, excluding leading/trailing trivia. *)
val span : 'a t -> 'a -> Span.t

(** Emit this term, using {!Emit}. *)
val emit : 'a t -> Format.formatter -> 'a -> unit
