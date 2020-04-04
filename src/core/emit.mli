(** Re-emit a syntax tree into Lua source code. *)

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

(** We extend the formatting semantic tags with {!token_kind}s, each {!Node.t} or {!Node.trivial}
    will be wrapped in tag of this type. *)
type Format.stag += Token of token_kind

(** Write a trivial term to the formatter. *)
val trivial : Format.formatter -> Node.trivial -> unit

(** Write an arbitrary node, accepting a {!token_kind} (used for highlighting) and a pretty-printer
    for the node's contents. *)
val node :
  kind:token_kind -> (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a Node.t -> unit

(** Write a token to the formatter. *)
val token : kind:token_kind -> Format.formatter -> token -> unit

(** Write a variable to the formatter. *)
val var : Format.formatter -> var -> unit

(** Write a name to the formatter. *)
val name : Format.formatter -> name -> unit

(** Write a function call to a formatter. *)
val call : Format.formatter -> call -> unit

(** Write an expression to a formatter. *)
val expr : Format.formatter -> expr -> unit

(** Write a statement to a formatter. *)
val stmt : Format.formatter -> stmt -> unit

(** Write a block to a formatter. *)
val block : Format.formatter -> block -> unit

(** Write a program to a formatter. *)
val program : Format.formatter -> program -> unit

(** Write a repl input to a formatter. *)
val repl_exprs : Format.formatter -> repl_exprs -> unit
