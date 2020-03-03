(** A basic system for defining incremental computations. *)

(** A global mapping of keys to values and a way to compute them. Constructed using
    {!Builder.build}. *)
type t

(** The context under which some value will be computed. This is similar to the main store, but with
    additional tracking. *)
type context

module Key : sig
  type ('k, 'v) t

  (** Get the name of this key *)
  val name : (_, _) t -> string

  (** A factory for a key, accepting a display name and some provider function ['f]. This accepts
      the following additional options:

      - [container_k]: A container to hold keys.
      - [eq_v]: A comparison function for values, to determine if two terms are equal. Defaults to
        the {!(==)} *)
  type ('k, 'v, 'f) factory =
    name:string ->
    ?container_k:(module Contained_tbl.KeyContainer with type t = 'k) ->
    ?eq_v:('v -> 'v -> bool) ->
    'f ->
    ('k, 'v) t

  (** A derived key, which uses the store (and thus other keys or the oracle) in order *)
  val key : ('k, 'v, context -> 'k -> 'v) factory

  (** Construct an "oracle" key. This provides data from outside the store (such as the filesystem).
      A provider for oracles must be registered with {!Builder.oracle}. *)
  val oracle : ('k, 'v, unit) factory
end

module Builder : sig
  type store := t

  type t

  (** An empty builder. *)
  val empty : t

  (** Register a provider for an oracle. Throws if the key was not constructed with {!Key.oracle},
      or a provider is already registered. *)
  val oracle : ('k, 'v) Key.t -> ('k -> 'v) -> t -> t

  (** Convert a store builder into a fully-fledged oracle. *)
  val build : t -> store
end

(** Mark this task as depending upon a value, and compute it. *)
val need : context -> ('k, 'v) Key.t -> 'k -> 'v

(** Query the store for a value, computing it if required. *)
val get : t -> ('k, 'v) Key.t -> 'k -> 'v

(** Increment the internal version number. This causes oracles to be re-queried. This should have no
    effect if oracles have not changed. *)
val refresh : t -> unit
