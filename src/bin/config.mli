open IlluaminateCore
open IlluaminateConfig

(** The configuration option for a set of files. This will contain some global options, and path
    specific information. *)
type t

(** Read config from a file, either accepting some options or producing a warning. *)
val of_file : Error.t -> Span.filename -> t option

(** Get the default config. *)
val default : t

(** Write the default config to a formatter. *)
val generate : Format.formatter -> unit

(** Determine if this file is included in the source list. *)
val is_source : t -> Fpath.t -> bool

(** Get the enabled error tags and linter options from the config object. *)
val get_linters : t -> Fpath.t -> Error.Tag.filter * Schema.store
