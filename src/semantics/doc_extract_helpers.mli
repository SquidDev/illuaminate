open IlluaminateCore
open Doc_syntax
module C := Doc_comment

module Tag : sig
  val type_mismatch : Error.Tag.t

  val kind_mismatch : Error.Tag.t

  val all : Error.Tag.t list
end

(** Convert a documentation comment into a value. *)
module Value : sig
  val get_documented :
    report:(Error.Tag.t -> Span.t -> string -> unit) -> C.comment -> value documented
end

(** Merge two terms. *)
module Merge : sig
  val documented : (Span.t -> 'a -> 'b -> 'c) -> 'a documented -> 'b documented -> 'c documented

  val value : errs:Error.t -> Span.t -> value -> value -> value

  val doc_value : errs:Error.t -> value documented -> value documented -> value documented

  val page : errs:Error.t -> Span.t -> page -> page -> page
end

(** Drop any local definitions in a value. *)
module DropLocal : sig
  val value : value -> value

  val mod_types : type_info documented list -> type_info documented list
end
