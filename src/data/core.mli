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

      This accepts the key, and the previous value, and returns the new value. *)
  val oracle : ('k, 'v, 'k -> 'v option -> 'v) factory

  (** Construct a deferred key, whose provider function can be specified when constructing the
      store.. The provider for such keys must be registered with {!Builder.key} or
      {!Builder.oracle}. *)
  val deferred : ('k, 'v, unit) factory
end

module Builder : sig
  type store := t

  type t

  (** An empty builder. *)
  val empty : t

  (** Register a provider for a deferred which just queries the store. This causes the deferred key
      to act as a standard key ({!Key.key}).

      Throws if the key was not constructed with {!Key.deferred}, or a provider is already
      registered. *)
  val key : ('k, 'v) Key.t -> (context -> 'k -> 'v) -> t -> t

  (** Register a provider for a deferred key. Throws if the key was not constructed with
      {!Key.deferred}, or a provider is already registered. *)
  val oracle : ('k, 'v) Key.t -> ('k -> 'v option -> 'v) -> t -> t

  (** Convert a store builder into a fully-fledged oracle. *)
  val build : t -> store
end

(** Mark this task as depending upon a value, and compute it. *)
val need : context -> ('k, 'v) Key.t -> 'k -> 'v

(** Query the store for a value, computing it if required. *)
val get : t -> ('k, 'v) Key.t -> 'k -> 'v

(** Run an arbitrary action within a context. This isn't generally recommended, but may be useful if
    you have functions which use {!need} instead of {!get} *)
val compute : (context -> 'a) -> t -> 'a

(** Increment the internal version number. This causes oracles to be re-queried. This should have no
    effect if oracles have not changed. *)
val refresh : t -> unit
