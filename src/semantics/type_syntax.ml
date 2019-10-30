module type S = sig
  type name

  type t =
    | NilTy
    | BoolTy of bool
    | IntTy of int
    | NumberTy of float
    | StringTy of string
    | Named of name
    | Function of
        { args : arg list;
          return : t list * t option
        }
    | Table of table_entry list
    | Union of t list
  [@@deriving show]

  and arg =
    { name : string option;
      ty : t;
      opt : bool
    }
  [@@deriving show]

  and table_entry =
    | Field of
        { key : string;
          value : t
        }
    | Array of t
    | Hash of
        { key : t;
          value : t
        }
  [@@deriving show]
end

module Make (X : sig
  type name

  val pp_name : Format.formatter -> name -> unit
end) =
struct
  type name = X.name

  let pp_name = X.pp_name

  type t =
    | NilTy
    | BoolTy of bool
    | IntTy of int
    | NumberTy of float
    | StringTy of string
    | Named of name
    | Function of
        { args : arg list;
          return : t list * t option
        }
    | Table of table_entry list
    | Union of t list
  [@@deriving show]

  and arg =
    { name : string option;
      ty : t;
      opt : bool
    }
  [@@deriving show]

  and table_entry =
    | Field of
        { key : string;
          value : t
        }
    | Array of t
    | Hash of
        { key : t;
          value : t
        }
  [@@deriving show]
end

module Unresolved = Make (struct
  type name = Reference.unresolved

  let pp_name = Reference.pp_unresolved
end)

module Resolved = Make (struct
  type name = Reference.resolved

  let pp_name = Reference.pp_resolved
end)
