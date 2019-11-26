(** Config categories represent a top-level collection of options.

    Unlike {!Term}, categories can be extended with additional options. However, they cannot be
    composed with each other, and so should be treated with care. *)
module Category : sig
  (** A config category. *)
  type t

  (** An entry in a config category, wrapping a {!Term}. *)
  type 'a key

  (** Create a new config category. This should be shared across the whole program. *)
  val create : ?parent:t -> name:string -> comment:string -> unit -> t

  (** Associate a term with a config category . *)
  val add : 'a Term.t -> t -> 'a key
end

(** A schema is merely a collection of config keys. *)
module Schema : sig
  (** Holds a collection of config items to read/write. *)
  type t

  (** An empty schema. *)
  val empty : t

  (** Create a schema from a single config key. *)
  val singleton : 'a Category.key -> t

  (** Combine multiple schemas together. *)
  val union : t -> t -> t

  (** A physical store of loaded options from a schema. *)
  type store

  (** Convert a schema store into a {!Term.t}, suitable for loading with {!Storage}. *)
  val to_term : t -> store Term.t

  (** Read the value of a key from this schema.

      Errors if this key is not within the schema. *)
  val get : 'a Category.key -> store -> 'a
end
