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

(** Write an expression to a formatter. *)
val expr : Format.formatter -> expr -> unit

(** Write a statement to a formatter. *)
val stmt : Format.formatter -> stmt -> unit

(** Write a program to a formatter. *)
val program : Format.formatter -> program -> unit
