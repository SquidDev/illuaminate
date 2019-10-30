type module_kind =
  | Module
  | Library

module type S = sig
  type reference

  module Type : Type_syntax.S

  type description = Description of Omd.t

  type see =
    { see_reference : reference;
      see_description : description option
    }

  type example =
    | RawExample of string
    | RichExample of description

  type arg =
    { arg_name : string;
      arg_opt : bool;
      arg_type : Type.t option;
      arg_description : description option
    }

  type return =
    { ret_type : Type.t option;
      ret_many : bool;
      ret_description : description option
    }

  type nonrec module_kind = module_kind =
    | Module
    | Library
end

module Make (X : sig
  type reference

  module Type : Type_syntax.S
end) : S with type reference = X.reference and module Type = X.Type = struct
  type reference = X.reference

  module Type = X.Type

  type description = Description of Omd.t

  type see =
    { see_reference : reference;
      see_description : description option
    }

  type example =
    | RawExample of string
    | RichExample of description

  type arg =
    { arg_name : string;
      arg_opt : bool;
      arg_type : Type.t option;
      arg_description : description option
    }

  type return =
    { ret_type : Type.t option;
      ret_many : bool;
      ret_description : description option
    }

  type nonrec module_kind = module_kind =
    | Module
    | Library
end
