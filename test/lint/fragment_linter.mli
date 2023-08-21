(** Lint a fragment, printing the error messages and fixed document to the given output. *)
val process : ?name:string -> string -> IlluaminateCore.Error.t * Diff.t option
