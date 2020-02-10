(** Execute a command (using the path), and either return STDOUT or a reason the command failed. *)
val exec : string -> string array -> (string, string) Result.t
