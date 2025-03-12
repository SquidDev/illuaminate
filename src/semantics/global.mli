(** Determine if an expression resolves to a specific global variable. *)

open IlluaminateCore

(** The name of a global variable. *)
type t

(** Parse a global, replacing it with a string. *)
val parse : string -> t

(** Extract a global variable from a variable. This {i must} be used on a usage, and not an
    assignment. *)
val of_var : Resolve.t -> Syntax.var -> t option

(** Extract a global variable from a name. This {i must} be used on a usage, and not an assignment.
*)
val of_name : Resolve.t -> Syntax.name -> t option

(** Extract a global variable from an expression. *)
val of_expr : Resolve.t -> Syntax.expr -> t option
