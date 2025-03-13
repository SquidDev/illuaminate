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
type Format.stag += Token of token_kind | Name of name | Var of var

(** A sink of nodes. *)
module type Emitter = sig
  type t

  (** Wrap a term in a formatting stag. *)
  val tagged : Format.stag -> (t -> 'a -> unit) -> t -> 'a -> unit

  (** Write an arbitrary node, accepting a {!token_kind} (used for highlighting) and a
      pretty-printer for the node's contents. *)
  val node : kind:token_kind -> (Format.formatter -> 'a -> unit) -> t -> 'a Node.t -> unit
end

module type S = sig
  include Emitter

  (** Write a token to the formatter. *)
  val token : kind:token_kind -> t -> token -> unit

  (** Write a variable to the formatter. *)
  val var : t -> var -> unit

  (** Write a name to the formatter. *)
  val name : t -> name -> unit

  (** Write a function call to a formatter. *)
  val call : t -> call -> unit

  (** Write a function call's arguments to a formatter. *)
  val call_args : t -> call_args -> unit

  (** Write a function definition's arguments to a formatter. *)
  val args : t -> args -> unit

  (** Write a table_item to a formatter. *)
  val table_item : t -> table_item -> unit

  (** Write a function name to a formatter. *)
  val function_name : t -> function_name -> unit

  (** Write an expression to a formatter. *)
  val expr : t -> expr -> unit

  (** Write a statement to a formatter. *)
  val stmt : t -> stmt -> unit

  (** Write a block to a formatter. *)
  val block : t -> block -> unit

  (** Write a program to a formatter. *)
  val program : t -> program -> unit

  (** Write a repl input to a formatter. *)
  val repl_exprs : t -> repl_exprs -> unit
end

module Make (E : Emitter) : S with type t := E.t

(** Write a trivial term to a formatter. *)
val trivial : Format.formatter -> Node.Trivia.t -> unit

(** By default we provide emitting functions which write to a formatter.*)
include S with type t := Format.formatter
