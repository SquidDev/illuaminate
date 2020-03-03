(** Utilities for working with programs and other code units. *)

open IlluaminateCore

(** Provides information about a given program's context. *)
module Context : sig
  type t =
    { root : Fpath.t;
          (** The root directory for this project. Namely, where the root [\[illuaminate.sexp\]]
              config is located. *)
      config : IlluaminateConfig.Schema.store  (** Configuration options for this file. *)
    }

  (** An oracle for fetching the context. This must be provided using {!Core.Builder.oracle}. *)
  val key : (Span.filename, t) Core.Key.t
end

module Files : sig
  (** A collection of files, and their corresponding program. Unlike the main data store, this is
      immutable. *)
  type t

  (** A unique identifier for a file. *)
  type id

  (** An oracle for fetching a file's contents. This must be provided using {!builder} (or your own
      implementation. *)
  val file : (id, Syntax.program) Core.Key.t

  (** An oracle which lists all files. This must be provided using {!builder} (or your own
      implementation. *)
  val files : (unit, id list) Core.Key.t

  (** Add an oracle for the given file collection to a context. *)
  val builder : t -> Core.Builder.t -> Core.Builder.t

  val create : unit -> t

  (** Add a new program to the file collection, returning the updated identifier for this file. You
      should call {!Core.refresh} after calling this. *)
  val add : Syntax.program -> t -> id

  (** Update a file's program. You should call {!Core.refresh} after calling this. *)
  val update : id -> Syntax.program -> unit
end

(** A key within the data store.

    This is used by {!get} to look up all associated information for a specific analysis pass. *)
type 'a key = (Syntax.program, 'a) Core.Key.t

(** Construct a new {!type:key} from some "metadata getter" function.

    Note that the "metadata generator" function can be lazy in generating data. *)
val key : name:string -> (Core.context -> Syntax.program -> 'a) -> 'a key
