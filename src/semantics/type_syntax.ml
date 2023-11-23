module type S = sig
  type reference

  type t =
    | NilTy
    | BoolTy of bool
    | IntTy of int
    | NumberTy of float
    | StringTy of string
    | Named of
        { ref : reference;
          label : string
        }
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

module Make (X : sig
  type reference
end) : S with type reference = X.reference = struct
  type reference = X.reference

  type t =
    | NilTy
    | BoolTy of bool
    | IntTy of int
    | NumberTy of float
    | StringTy of string
    | Named of
        { ref : reference;
          label : string
        }
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

module Unresolved = Make (struct
  type reference = Reference.unresolved
end)

module Resolved = Make (struct
  type reference = Reference.resolved
end)

module Lift (L : S) (R : S) = struct
  let t of_ref : L.t -> R.t =
    let rec ty : L.t -> R.t = function
      | NilTy -> NilTy
      | BoolTy x -> BoolTy x
      | IntTy x -> IntTy x
      | NumberTy x -> NumberTy x
      | StringTy x -> StringTy x
      | Named { ref; label } -> Named { ref = of_ref ref; label }
      | Function { args; return = rs, r } ->
          Function { args = List.map arg args; return = (List.map ty rs, Option.map ty r) }
      | Table ts -> Table (List.map table_entry ts)
      | Union ts -> Union (List.map ty ts)
    and arg { L.name; ty = t; opt } = { R.name; ty = ty t; opt }
    and table_entry : L.table_entry -> R.table_entry = function
      | Field { key; optional; value } -> Field { key; optional; value = ty value }
      | Item t -> Item (ty t)
      | Many t -> Many (ty t)
      | Hash { key; optional; value } -> Hash { key = ty key; optional; value = ty value }
    in
    ty
end

module Builtin = struct
  open Resolved

  let mk_external name =
    Named
      { ref = External { name; url = Lua_reference.(lookup_type name |> to_url) }; label = name }

  let string = mk_external "string"
  let number = mk_external "number"
  let boolean = mk_external "number"
end
