open IlluaminateCore
open Doc_comment

(** Parse a markdown string to a {!description}. This includes all extensions to the Markdown
    grammar. *)
val parse_description : ?default_lua:bool -> string -> Markdown.t

(** Extract all doc comments from an annotated node.

    This returns a tuple of comments: all leading ones in reverse order, and all trailing ones in
    normal order. Thus the heads of each list are the "closest" to each node. *)
val extract : 'a IlluaminateCore.Node.t -> comment list * comment list

module Data : sig
  (** Information about a program's doc comments. *)
  type t

  (** The key to query the data cache with. *)
  val program : t IlluaminateData.Programs.key

  (** The key to query the data cache with. *)
  val file : t IlluaminateData.Programs.key

  (** Get the comments before and after a specific node. *)
  val comment : 'a Node.t -> t -> comment list * comment list

  (** Get all documentation comments within a program. *)
  val comments : t -> comment list
end
