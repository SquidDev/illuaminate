(** Utilities for working with Lua identifiers. *)

(** A set of Lua keywords. *)
val keywords : Set.Make(String).t

(** Is this string a valid identifier? *)
val is : string -> bool
