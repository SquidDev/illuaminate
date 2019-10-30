type module_kind =
  | Module
  | Library

module type S = sig
  type reference

  module Type : Type_syntax.S

  type description = Description of Omd.t

  (** A link to another name. *)
  type see =
    { see_reference : reference;  (** The name this see link points to. *)
      see_description : description option  (** An optional description of this name. *)
    }

  (** An example of how to use this code. *)
  type example =
    | RawExample of string  (** An example with no associated description. *)
    | RichExample of description  (** A rich markdown block, with some associated comments. *)

  (** An argument to a function. *)
  type arg =
    { arg_name : string;  (** The argument's name. *)
      arg_opt : bool;  (** Whether the argument is optional or not. *)
      arg_type : Type.t option;  (** The argument's type. *)
      arg_description : description option  (** An additional description of the argument. *)
    }

  (** A return value of a function. *)
  type return =
    { ret_type : Type.t option;  (** The type of this return value. *)
      ret_many : bool;
          (** Whether this may return multiple values. Should only be used on the last one. *)
      ret_description : description option  (** An additional description of the return value. *)
    }

  type nonrec module_kind = module_kind =
    | Module
    | Library
end

module Make (X : sig
  type reference

  module Type : Type_syntax.S
end) : S with type reference := X.reference and module Type = X.Type
