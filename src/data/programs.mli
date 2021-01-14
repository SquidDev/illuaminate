(** Utilities for working with programs and other code units. *)

open IlluaminateCore

(** Provides information about a given program's context. *)
module Context : sig
  type t =
    { root : Fpath.t option;
          (** The root directory for this project. Namely, where the root [\[illuaminate.sexp\]]
              config is located. This may be empty, if the file doesn't correspond to a location on
              disk. *)
      config : IlluaminateConfig.Schema.store  (** Configuration options for this file. *)
    }

  (** An oracle for fetching the context. This must be provided using {!Core.Builder.oracle}. *)
  val key : (Span.filename, t) Core.Key.t
end

module Files : sig
  (** Fetch a given file's contents.

      This may be registered using {!FileStore.builder}, or your own implementation. *)
  val file : (Span.filename, File.t option) Core.Key.t

  (** List all available files. This may be registered using {!FileStore.builder}, or your own
      implementation.*)
  val files : (unit, Span.filename list) Core.Key.t
end

module FileStore : sig
  (** A collection of files and their corresponding program. *)
  type t

  val create : unit -> t

  (** Add implementations for the {!Files} keys using the given file store. *)
  val builder : t -> Core.Builder.t -> Core.Builder.t

  (** Add implementations for the {!Files} keys using the given file store. *)
  val lazy_builder : t Lazy.t -> Core.Builder.t -> Core.Builder.t

  (** Update a file's program. You should call {!Core.refresh} after calling this. *)
  val update : t -> Span.filename -> File.t option -> unit
end

(** A key within the data store.

    This is used by {!get} to look up all associated information for a specific analysis pass. *)
type 'a key = (Syntax.program, 'a) Core.Key.t

(** Construct a new {!type:key} from some "metadata getter" function.

    Note that the "metadata generator" function can be lazy in generating data. *)
val key : name:string -> (Core.context -> Syntax.program -> 'a) -> 'a key

(** Wraps {!Core.need}, first looking up a program for the current filename. *)
val need_for : Core.context -> (Syntax.program, 'a) Core.Key.t -> Span.filename -> 'a option

(** Wraps {!Core.get}, first looking up a program for the current filename. *)
val get_for : Core.t -> (Syntax.program, 'a) Core.Key.t -> Span.filename -> 'a option

(** Construct a new key from some "metadata getter" function.

    Note that the "metadata generator" function can be lazy in generating data. *)
val file_key : name:string -> (Core.context -> File.t -> 'a) -> (File.t, 'a) Core.Key.t
