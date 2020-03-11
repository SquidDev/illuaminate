module Control = Control
module Global = Global
module Pure = Pure
module Reference = Reference
module Resolve = Resolve
module Module_resolve = Module_resolve

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

(** Functions relating to Lua's [string] library. *)
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
end
