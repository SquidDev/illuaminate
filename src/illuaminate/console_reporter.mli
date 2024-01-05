(** Reports a list of errors ({!Error.t}) to the console. *)

(** Display a list of errors, supplying the file's contents from disk. *)
val display_of_files : ?out:Format.formatter -> ?with_summary:bool -> Error.t list -> unit

(** Display a list of errors, supplying the file's contents as a string. *)
val display_of_string :
  ?out:Format.formatter ->
  ?with_summary:bool ->
  (File_id.t -> string option) ->
  Error.t list ->
  unit
