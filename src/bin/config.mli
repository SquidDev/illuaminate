open IlluaminateCore
open IlluaminateConfig
open IlluaminateLint

type t

val parser : t Parser.t

val default : t

val generate : Format.formatter -> unit -> unit

val get_linters : t -> string -> Linter.t list * Error.Tag.t list * Schema.store
