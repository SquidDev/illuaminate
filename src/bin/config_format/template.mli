type t

val parse : string list -> string -> (t, string) CCResult.t
val apply : (string -> string option) -> t -> string option
val pp : Format.formatter -> t -> unit
val converter : string list -> (string -> (t, string) CCResult.t) * (t -> string)
