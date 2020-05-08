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

      - [container]: A container to hold keys.
      - [eq]: A comparison function for values, to determine if two terms are equal. Defaults to the
        {!(==)} *)
  type ('k, 'v, 'f) factory =
    name:string ->
    ?pp:(Format.formatter -> 'k -> unit) ->
    ?container:(module Contained_tbl.KeyContainer with type t = 'k) ->
    'f ->
    ('k, 'v) t

  (** The reason this builtin key was invoked. *)
  type 'a reason =
    | Absent  (** The key was not computed before. *)
    | DependencyChange of 'a  (** A dependency has changed for some reason. *)
    | Recompute of 'a
        (** No dependencies have changed. We're simply recomputing due to {!refresh} having been
            called. *)

  (** The kind of change that has occurred. *)
  type change =
    | NoChange
    | RecomputeChange
    | RecomputeSame

  type 'a result =
    { value : 'a;
      changed : change
    }

  (** Construct an "builtin" key. This fetches information from both the outside world and the store
      in order to compute a value.

      This accepts the key and the reason the builtin is computed. Returns the new value. *)
  val builtin : ('k, 'v, context -> 'k -> 'v reason -> 'v result) factory

  (** A basic key. This just uses the store in order to compute its value. It does not depend on the
      outside world.*)
  val key : ?eq:('v -> 'v -> bool) -> ('k, 'v, context -> 'k -> 'v) factory

  (** A basic oracle. This simply fetches from the outside world, without caring about dependencies. *)
  val oracle : ?eq:('v -> 'v -> bool) -> ('k, 'v, 'k -> 'v option -> 'v) factory

  (** Construct a deferred key, whose provider function can be specified when constructing the
      store.. The provider for such keys must be registered with {!Builder.key} or
      {!Builder.builtin}.

      The supplied [eq] function is only used for non-"builtin" keys. *)
  val deferred : ?eq:('v -> 'v -> bool) -> ('k, 'v, unit) factory
end

module Builder : sig
  type store := t

  type t

  (** An empty builder. *)
  val empty : t

  (** Register a provider for a deferred key which just queries the store. This causes the deferred
      key to act as a standard key ({!Key.key}).

      Throws if the key was not constructed with {!Key.deferred}, or a provider is already
      registered. *)
  val key : ('k, 'v) Key.t -> (context -> 'k -> 'v) -> t -> t

  (** Register a provider for a deferred key. Throws if the key was not constructed with
      {!Key.deferred}, or a provider is already registered. *)
  val builtin : ('k, 'v) Key.t -> (context -> 'k -> 'v Key.reason -> 'v Key.result) -> t -> t

  val oracle : ('k, 'v) Key.t -> ('k -> 'v option -> 'v) -> t -> t

  (** Convert a store builder into a fully-fledged store. *)
  val build : t -> store
end

(** Mark this task as depending upon a value, and compute it. *)
val need : context -> ('k, 'v) Key.t -> 'k -> 'v

(** Query the store for a value, computing it if required. *)
val get : t -> ('k, 'v) Key.t -> 'k -> 'v

(** Run an arbitrary action within a context. This isn't generally recommended, but may be useful if
    you have functions which use {!need} instead of {!get} *)
val compute : (context -> 'a) -> t -> 'a

(** Increment the internal version number. This causes builtin keys to be recomputed. This should
    have no effect if keys have not changed. *)
val refresh : t -> unit
