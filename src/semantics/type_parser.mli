(** Wraps {!Type_grammar} and provides a mechanism to easily parse it. *)

(** Parse a type string *)
val parse : string -> (Type_syntax.Unresolved.t, string) Result.t

(** Parse a type string *)
val parse_vararg : string -> (bool * Type_syntax.Unresolved.t, string) Result.t
