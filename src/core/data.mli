(** Provides a way of associating {!Syntax} programs with metadata. *)

(* TODO: How do we handle invalidation in a multi-program setting? *)

(** A cache of data for specific programs. *)
type t

(** Construct a new store. *)
val create : unit -> t

(** A key within the data store.

    This is used by {!get} to look up all associated information for a specific analysis pass. *)
type 'a key

(** Construct a new {!type:key} from some "metadata getter" function. *)
val key : name:string -> (t -> Syntax.program -> 'a) -> 'a key

(** Get a program's metadata from the store. *)
val get : Syntax.program -> 'a key -> t -> 'a
