open IlluaminateCore
open IlluaminateConfig

(** The configuration option for a set of files. This will contain some global options, and path
    specific information. *)
type t

type doc_options =
  { site_title : string option;  (** A title to be displayed on every page. *)
    index : Fpath.t option;
        (** A path to a [\[.html\]] or [\[.md\]] file to use on the index page. *)
    destination : Fpath.t  (** Destination folder to write to. *)
  }

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

(** Get options relating to the documentation. *)
val get_doc_options : t -> doc_options

(** Get the global data store. *)
val get_store : t -> Schema.store
