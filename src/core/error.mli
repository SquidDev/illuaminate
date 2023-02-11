(** The underlying system for declaring and reporting errors.

    Every error reported by illuaminate has an associated {!Tag.t}, which defines the error level
    (error, warning, etc...) and gives it a friendly name. Tags can then be used to enable/disable
    particular errors or groups of errors.

    Errors are then reported into an error sink ({!t}), which counts errors, sorts them into files,
    and is used to report them (see {!display_of_files}).*)

(** Attributes about an error's tag (and thus errors using that tag). *)
type attribute =
  | Default  (** This tag is enabled by default, when used in a linter. *)
  | Unused
      (** This tag warns about some unused node, such as an unused variable or unreachable code.

          This maps to LSP's "unnecessary" diagnostic tag. *)
  | Deprecated
      (** This tag warns about working with a deprecated node.

          This maps to LSP's "deprecated" diagnostic tag. *)

(** An error reporting level *)
type level =
  | Critical  (** An error which cannot be recovered from *)
  | Error  (** A problem, which may cause problems during processing or runtime *)
  | Warning  (** A potential problem or bug. *)
  | Note  (** A minor issue. Effectively just a less major warning. *)

module Tag : sig
  (** A marker token for errors, used for filtering and classification *)
  type t = private
    { name : string;
      level : level;
      attributes : attribute list
    }

  (** Display a tag in a formatter. *)
  val pp : Format.formatter -> t -> unit

  (** Create a new error tag, and register it internally *)
  val make : attr:attribute list -> level:level -> string -> t

  (** Find a tag with a given name *)
  val find : string -> t option

  (** Compare two tags. *)
  val compare : t -> t -> int

  (** Determine if this tag has an attribute. *)
  val has : attribute -> t -> bool

  (** A predicate which determines if a tag should be reported or not. *)
  type filter = t -> bool
end

module Error : sig
  (** An error reported within the system. *)
  type t = private
    { tag : Tag.t;
      span : Span.t;
      message : string;
      details : (Format.formatter -> unit) option
    }

  (** Compare two errors by their position. This is suitable for sorting, but obviously does not
      determine equality. *)
  val span_compare : t -> t -> int
end

(** An error reporting writer, into which errors are placed *)
type t

(** Make a new error reporter *)
val make : unit -> t

(** Report a new error at a specific position *)
val report : t -> Tag.t -> Span.t -> string -> unit

(** Report a new error at a specific position with additional details.

    The detail printer may use the styles provided by {!Style} to provide richer messages. *)
val report_detailed : t -> Tag.t -> Span.t -> string -> (Format.formatter -> unit) -> unit

(** Determine if the error reporter has any problems in it. *)
val has_problems : t -> bool

(** Return a sorted list of errors and their locations. *)
val errors : t -> Error.t list

(** Display any errors which occurred. This assumes files exist on disk - use {!display_of_string}
    if this is not the case. *)
val display_of_files : ?out:Format.formatter -> ?with_summary:bool -> t -> unit

(** Display any errors which occurred, with a function which maps file names to strings *)
val display_of_string :
  ?out:Format.formatter -> ?with_summary:bool -> (Span.filename -> string option) -> t -> unit
