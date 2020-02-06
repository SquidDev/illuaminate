module type S = sig
  type reference

  type t =
    | NilTy
    | BoolTy of bool
    | IntTy of int
    | NumberTy of float
    | StringTy of string
    | Named of reference * string
    | Function of
        { args : arg list;
          return : t list * t option
        }
    | Table of table_entry list
    | Union of t list

  and arg =
    { name : string option;
      ty : t;
      opt : bool
    }

  and table_entry =
    | Field of
        { key : string;
          optional : bool;
          value : t
        }
    | Item of t
    | Many of t
    | Hash of
        { key : t;
          optional : bool;
          value : t
        }
end

(** Construct an arbitrary *)
module Make (X : sig
  type reference
end) : S with type reference = X.reference

(** Lift a type from using one reference to using another. *)
module Lift (L : S) (R : S) : sig
  val t : (L.reference -> R.reference) -> L.t -> R.t
end

module Unresolved : S with type reference = Reference.unresolved

module Resolved : S with type reference = Reference.resolved

module Builtin : sig
  open Resolved

  (** Lua's builtin string type. *)
  val string : t

  (** Lua's builtin number type. *)
  val number : t

  (** Lua's builtin boolean type. *)
  val boolean : t
end
