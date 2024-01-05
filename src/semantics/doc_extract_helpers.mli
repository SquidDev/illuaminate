open IlluaminateCore
open Doc_syntax
module C := Doc_comment

type extract_error =
  | Value_mismatch of Span.t * value * value
  | Func_and_type of Span.t
  | Func_and_field of Span.t

(** Convert a documentation comment into a value. *)
module Value : sig
  val debug_name : value -> string
  val get_documented : report:(extract_error -> unit) -> C.comment -> value documented
end

(** Merge two terms. *)
module Merge : sig
  val documented : (Span.t -> 'a -> 'b -> 'c) -> 'a documented -> 'b documented -> 'c documented
  val value : report:(extract_error -> unit) -> Span.t -> value -> value -> value

  val doc_value :
    report:(extract_error -> unit) -> value documented -> value documented -> value documented

  val page : report:(extract_error -> unit) -> Span.t -> page -> page -> page
end

(** Drop any local definitions in a value. *)
module DropLocal : sig
  val value : value -> value
  val mod_types : type_info documented list -> type_info documented list
end
