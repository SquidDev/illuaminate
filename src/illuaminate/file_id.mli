(** A file, with both a display name and absolute path *)

type t = private
  { name : string;  (** A "display" name of the file, which can be shown to the user. *)
    path : Fpath.t option;  (** An absolute path this file may exist at. *)
    id : string;
        (** A unique identifier for this file. This may be the path, or some other piece of data.

            Generally the meaning/contents of this will vary depending on what program/library is
            constructing the spans, and thus should not be understood to have any specific meaning. *)
    hash : int  (** A cached hash of [id] *)
  }

val pp : Format.formatter -> t -> unit

include Hashtbl.HashedType with type t := t
include Set.OrderedType with type t := t

(** Construct a new filename from a unique id, with an optional on-disk file and display name. *)
val mk : ?path:Fpath.t -> ?name:string -> string -> t
