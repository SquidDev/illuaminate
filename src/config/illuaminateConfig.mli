(** {!IlluaminateConfig} provides a system for reading and writing configuration files. *)

module Parser = Parser

(** Represents a configurable term. Unlike the more general {!Parser}, terms represent a simpler
    hierarchy of named fields, with the advantage that we can automatically generate a file from the
    desired configuration.

    Terms are built from a collection of key-value pairs, or fields (see {!Term.field}), which may
    be placed into sub-categories known as groups ({!Term.group}). Both fields and groups are named,
    and have an associated comment, which describes its purpose.

    Each field is built from a primitive type, which is read and written using a {!Converter}. More
    complex types should be built up using multiple fields and the various {!Term} combinators. *)
module Term : sig
  (** Provides handlers for converting primitive types to and from a config representation.

      More complex types should be built up using {!Term} instead. *)
  module Converter : sig
    (** A converter to and from the config file representation. *)
    type 'a t

    val bool : bool t

    val string : string t

    val float : float t

    val int : int t

    val list : 'a t -> 'a list t

    val enum : ty:string -> (string * 'a) list -> 'a t

    val atom : ty:string -> (string -> ('a, string) result) -> ('a -> string) -> 'a t
  end

  type 'a t

  (** Build a group out of one or more child fields. *)
  val group : name:string -> comment:string -> 'a t -> 'a t

  (** Build a field from a converter. This is the "leaf" node of the config object tree. *)
  val field : name:string -> comment:string -> default:'a -> 'a Converter.t -> 'a t

  (** An empty term which does nothing. *)
  val unit : unit t

  (** A term which always yields a value. *)
  val const : 'a -> 'a t

  (** Get the default value of this term. *)
  val default : 'a t -> 'a

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  (** Write the default value to a formatter. *)
  val write_default : Format.formatter -> 'a t -> unit

  (** Convert this term into a parser, allowing you to read the config from an object or a file. *)
  val to_parser : 'a t -> 'a Parser.fields
end

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

(** A schema is merely a collection of {!Category} keys. These are then parsed into store, from
    which the key's data can be extracted. *)
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

  (** Convert a schema store into a {!Parser.fields}. *)
  val to_parser : t -> store Parser.fields

  (** Get the default schema store. *)
  val default : t -> store

  (** Write the default value to a formatter. *)
  val write_default : Format.formatter -> t -> unit

  (** Read the value of a key from this schema.

      Errors if this key is not within the schema. *)
  val get : 'a Category.key -> store -> 'a

  (** Perform the right-biased merge of two stores. Note this performs a recursive merge, meaning
      individual config nodes can be overridden. *)
  val merge : store -> store -> store
end
