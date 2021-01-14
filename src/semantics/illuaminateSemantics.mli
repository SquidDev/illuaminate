open IlluaminateCore
module Control = Control
module Global = Global
module Pure = Pure
module Reference = Reference
module Resolve = Resolve
module Module_resolve = Module_resolve
module Namespace = Namespace

(** Documentation comment parsing and processing *)
module Doc : sig
  module AbstractSyntax = Doc_abstract_syntax
  module Comment = Doc_comment
  module Parser = Doc_parser
  module Syntax = Doc_syntax
  module Extract = Doc_extract
end

(** Type parsing and resolution. *)
module Type : sig
  module Syntax = Type_syntax
end

(** Functions relating to Lua's [string] library and literals. *)
module Stringlib : sig
  (** Parse [string.format] format strings. *)
  module Format : sig
    (** A format specifier. *)
    type specifier =
      | Eof  (** An unfinished specifier. Namely a raw [%] just before the end of a string *)
      | Unknown of char  (** An unknown format specifier. *)
      | Known of string * char  (** A format specifier with a series of flags. *)
      | Raw of string  (** A raw string which will be included directly. *)

    type t = specifier list

    (** Parse a format specifier. *)
    val parse : string -> t
  end

  (** Parse strings and their internal escape sequences. *)
  module Literal : sig
    type component =
      | Segment of string  (** A raw string, without any quotes or escape sequences. *)
      | Escape of string * char
          (** A string escape, devised of the escape sequence (including ["\\"]) and the actual
              character. *)
      | Malformed of char * Span.t  (** A malformed string escape. *)
      | Quote of char  (** A string quote, namely [''] or [""]. *)

    type t = component list

    (** Parse a string literal. *)
    val parse : string Node.t -> t option
  end
end

module Ident : sig
  (** A set of Lua keywords. *)
  val keywords : Set.Make(String).t

  (** Is this string a valid identifier? *)
  val is : string -> bool
end
