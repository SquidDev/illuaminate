(** Provides a way of associating {!Syntax} programs with metadata. *)

open IlluaminateCore

module Files : sig
  (** A collection of files, and their corresponding program. Unlike the main data store, this is
      immutable. *)
  type t

  (** A unique identifier for a file. *)
  type id

  (** Construct a new file collection. *)
  val create : unit -> t

  (** Add a new program to the file collection, returning the updated identifier for this file. *)
  val add : Syntax.program -> t -> t * id

  (** Update a file's program. *)
  val update : id -> Syntax.program -> t -> t
end

(** A cache of data for specific programs. *)
type t

(** Get the internal data store for a collection of files. This data store is (mostly) shared, even
    when the underlying {!files} list is shared, so any file/program specific data is shared. *)
val of_files : Files.t -> t

(** A key within the data store.

    This is used by {!get} to look up all associated information for a specific analysis pass. *)
type 'a key

(** Construct a new {!type:key} from some "metadata getter" function.

    Note that the "metadata generator" function can be lazy in generating data. *)
val key : name:string -> (t -> Syntax.program -> 'a) -> 'a key

(** Get a program's metadata from the store. *)
val get : Syntax.program -> 'a key -> t -> 'a

(** Get all files within this data store. *)
val files : t -> Files.id list

(** Get data for a specific file. *)
val get_for : Files.id -> 'a key -> t -> 'a
