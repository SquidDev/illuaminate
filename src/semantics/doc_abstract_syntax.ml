open IlluaminateCore

type module_kind =
  | MKModule
  | MKLibrary
  | MKNone

type position =
  { path : string;
    start_line : int;
    end_line : int
  }

type source =
  | Span of Span.t
  | Position of position

type change_kind =
  | Added
  | Changed

module Omd' = struct
  let iter = Omd_transform.Iter.doc

  let iter_code_blocks f = List.iter (Omd_transform.Iter.code_blocks f)
end

module type S = sig
  type reference

  module Type : Type_syntax.S with type reference = reference

  type description =
    { description : reference Omd.doc;
      description_pos : Span.t option
    }

  type nonrec module_kind = module_kind =
    | MKModule
    | MKLibrary
    | MKNone

  type nonrec change_kind = change_kind =
    | Added
    | Changed

  type see =
    { see_reference : reference;
      see_label : string;
      see_span : Span.t;
      see_description : description option
    }

  type deprecation = { deprecation_message : description option } [@@unboxed]

  type example =
    | RawExample of string Span.spanned
    | RichExample of description

  type opt_arg =
    | Required
    | Optional
    | Default of string

  type arg =
    { arg_name : string;
      arg_opt : opt_arg;
      arg_type : Type.t option;
      arg_description : description option
    }

  type return =
    { ret_type : Type.t option;
      ret_many : bool;
      ret_description : description option
    }

  type nonrec position = position =
    { path : string;
      start_line : int;
      end_line : int
    }

  type change =
    { change_kind : change_kind;
      change_version : string;
      change_span : Span.t;
      change_description : description option
    }

  type changes = change list

  class abstract_iter :
    object
      method reference : reference -> unit

      method description : description -> unit

      method type_ : Type.t -> unit

      method see : see -> unit

      method deprecation : deprecation -> unit

      method example : example -> unit

      method arg : arg -> unit

      method return : return -> unit

      method change : change -> unit
    end
end

module Make (X : sig
  type reference

  module Type : Type_syntax.S with type reference = reference
end) : S with type reference = X.reference and module Type = X.Type = struct
  type reference = X.reference

  module Type = X.Type

  type description =
    { description : reference Omd.doc;
      description_pos : Span.t option
    }

  type nonrec module_kind = module_kind =
    | MKModule
    | MKLibrary
    | MKNone

  type nonrec change_kind = change_kind =
    | Added
    | Changed

  type see =
    { see_reference : reference;
      see_label : string;
      see_span : Span.t;
      see_description : description option
    }

  type deprecation = { deprecation_message : description option } [@@unboxed]

  type example =
    | RawExample of string Span.spanned
    | RichExample of description

  type opt_arg =
    | Required
    | Optional
    | Default of string

  type arg =
    { arg_name : string;
      arg_opt : opt_arg;
      arg_type : Type.t option;
      arg_description : description option
    }

  type return =
    { ret_type : Type.t option;
      ret_many : bool;
      ret_description : description option
    }

  type nonrec position = position =
    { path : string;
      start_line : int;
      end_line : int
    }

  type change =
    { change_kind : change_kind;
      change_version : string;
      change_span : Span.t;
      change_description : description option
    }

  type changes = change list

  class abstract_iter =
    object (self)
      method reference (_ : reference) = ()

      method description (_ : description) = ()

      method type_ (_ : Type.t) = ()

      method see { see_reference; see_label = _; see_span = _; see_description } =
        self#reference see_reference;
        Option.iter self#description see_description

      method deprecation { deprecation_message } = Option.iter self#description deprecation_message

      method example =
        function
        | RawExample _ -> ()
        | RichExample d -> self#description d

      method arg { arg_name = _; arg_opt = _; arg_type; arg_description } =
        Option.iter self#type_ arg_type;
        Option.iter self#description arg_description

      method return { ret_type; ret_many = _; ret_description } =
        Option.iter self#type_ ret_type;
        Option.iter self#description ret_description

      method change { change_description; change_kind = _; change_span = _; change_version = _ } =
        Option.iter self#description change_description
    end
end

module Lift (L : S) (R : S) = struct
  type t =
    { any_ref : L.reference -> R.reference;
      type_ref : L.reference -> R.reference
    }

  let description x { L.description; description_pos } =
    { R.description = Omd_transform.Map.doc x.any_ref description; description_pos }

  let example lift : L.example -> R.example = function
    | RawExample e -> RawExample e
    | RichExample d -> RichExample (description lift d)

  let see lift { L.see_reference; see_label; see_span; see_description } =
    { R.see_reference = lift.any_ref see_reference;
      see_label;
      see_span;
      see_description = Option.map (description lift) see_description
    }

  let deprecation lift { L.deprecation_message } =
    { R.deprecation_message = Option.map (description lift) deprecation_message }

  module Ty_lift = Type_syntax.Lift (L.Type) (R.Type)

  let ty x = Ty_lift.t x.type_ref

  let opt_arg : L.opt_arg -> R.opt_arg = function
    | Required -> Required
    | Optional -> Optional
    | Default x -> Default x

  let arg lift { L.arg_name; arg_opt; arg_type; arg_description } =
    { R.arg_name;
      arg_opt = opt_arg arg_opt;
      arg_type = Option.map (ty lift) arg_type;
      arg_description = Option.map (description lift) arg_description
    }

  let return lift { L.ret_type; ret_many; ret_description } =
    { R.ret_type = Option.map (ty lift) ret_type;
      ret_many;
      ret_description = Option.map (description lift) ret_description
    }

  let change lift { L.change_kind; change_version; change_span; change_description } =
    { R.change_kind;
      change_version;
      change_span;
      change_description = Option.map (description lift) change_description
    }
end
