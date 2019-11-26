(** Represents a configurable term. Unlike the more general {!Parser}, terms represent a simpler
    hierarchy of named fields, with the advantage that we can automatically generate a file from the
    desired configuration. *)

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
