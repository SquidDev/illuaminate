open IlluaminateCore
open Doc_comment

(** A flag for a specific tag. *)
type doc_flag =
  | Marker of string
  | Named of string * string

(** Parse a markdown string to a {!description}. This includes all extensions to the Markdown
    grammar. *)
val parse_description : ?default_lang:string -> string -> description

(** Parse a documentation comment. *)
val parse : string -> string * (string * doc_flag list * string) list

(** Build a parsed comment into a {!doc_comment} *)
val build : Span.t -> string * (string * doc_flag list * string) list -> comment

(** Extract all doc comments from an annotated node.

    This returns a tuple of comments: all leading ones in reverse order, and all trailing ones in
    normal order. Thus the heads of each list are the "closest" to each node. *)
val extract : 'a IlluaminateCore.Node.t -> comment list * comment list

module Data : sig
  (** Information about a program's doc comments. *)
  type t

  (** The key to query the data cache with. *)
  val key : t Data.key

  (** Get the comments before and after a specific node. *)
  val comment : 'a Node.t -> t -> comment list * comment list

  (** Get all documentation comments within a program. *)
  val comments : t -> comment list
end

(** A set of all tags that may result from parsing. *)
module Tag : sig
  val malformed_tag : IlluaminateCore.Error.Tag.t

  val malformed_type : IlluaminateCore.Error.Tag.t

  val unknown_flag : IlluaminateCore.Error.Tag.t

  val unknown_tag : IlluaminateCore.Error.Tag.t

  val duplicate_definitions : IlluaminateCore.Error.Tag.t

  val bad_index : IlluaminateCore.Error.Tag.t

  val wrong_throws : IlluaminateCore.Error.Tag.t

  val all : IlluaminateCore.Error.Tag.t list
end
