(** A tiny diff library. *)

type t

(** Make a diff from a two multi-line strings. *)
val diff : old:string -> new_:string -> t

(** Display this diff. *)
val pp : Format.formatter -> t -> unit
