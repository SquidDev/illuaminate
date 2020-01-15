module Control = Control
module Global = Global
module Pure = Pure
module Reference = Reference
module Resolve = Resolve

(** Documentation comment parsing and processing *)
module Doc : sig
  module Comment = Doc_comment
  module Parser = Doc_parser
end

(** Type parsing and resolution. *)
module Type : sig
  module Syntax = Type_syntax
end

module Stringlib : sig
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
