open IlluaminateCore
open IlluaminateConfig

(** The configuration option for a set of files. This will contain some global options, and path
    specific information. *)
type t

type doc_options =
  { site_title : string option;  (** A title to be displayed on every page. *)
    index : Fpath.t option;
        (** A path to a [\[.html\]] or [\[.md\]] file to use on the index page. *)
    destination : Fpath.t;  (** Destination folder to write to. *)
    source_link : Span.t -> string option;
        (** Resolve a position in source code to an online file host (such as GitHub). *)
    json_index : bool
        (** Whether a JSON file containing an index of all definitions should be emitted. *)
  }

(** Read config from a lexer, either accepting some options or producing a warning. *)
val of_lexer :
  directory:Fpath.t -> Span.filename -> Lexing.lexbuf -> (t, string Span.spanned) result

(** Read config from a file, either accepting some options or producing a warning. *)
val of_file : Error.t -> Span.filename -> t option

(** Get the default config. *)
val default : t

(** Write the default config to a formatter. *)
val generate : Format.formatter -> unit

(** Iterator over files within this a subdirectory of this module. *)
val files : (Fpath.t -> unit) -> t -> Fpath.t -> unit

(** Iterator over source files within this module. *)
val all_files : (Fpath.t -> unit) -> t -> unit

(** Get the enabled error tags and linter options from the config object. *)
val get_linters : t -> ?path:Fpath.t -> unit -> Error.Tag.filter * Schema.store

(** Get options relating to the documentation. *)
val get_doc_options : t -> doc_options

(** Get the global data store. *)
val get_store : t -> Schema.store
