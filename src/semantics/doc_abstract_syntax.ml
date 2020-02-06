type module_kind =
  | Module
  | Library

module type S = sig
  type reference

  module Type : Type_syntax.S with type reference = reference

  type description = Description of Omd.t

  type see =
    { see_reference : reference;
      see_label : string;
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

  class abstract_iter :
    object
      method reference : reference -> unit

      method description : description -> unit

      method omd : Omd.element -> unit

      method type_ : Type.t -> unit

      method see : see -> unit

      method example : example -> unit

      method arg : arg -> unit

      method return : return -> unit
    end
end

module Make (X : sig
  type reference

  module Type : Type_syntax.S with type reference = reference
end) : S with type reference = X.reference and module Type = X.Type = struct
  type reference = X.reference

  module Type = X.Type

  type description = Description of Omd.t

  type see =
    { see_reference : reference;
      see_label : string;
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

  class abstract_iter =
    object (self)
      method reference (_ : reference) = ()

      method description (Description d) = List.iter self#omd d

      method omd =
        let open Omd in
        function
        | H1 x | H2 x | H3 x | H4 x | H5 x | H6 x | Paragraph x | Emph x | Bold x ->
            List.iter self#omd x
        | Url (_, x, _) | Blockquote x | Html (_, _, x) | Html_block (_, _, x) ->
            List.iter self#omd x
        | Ul xs | Ol xs | Ulp xs | Olp xs -> List.iter (List.iter self#omd) xs
        | Ref (_, _, _, f) | Img_ref (_, _, _, f) -> List.iter self#omd f#to_t
        | Text _ | Code _ | Code_block _ | Br | Hr | NL | Html_comment _ | Raw _ | Raw_block _
        | Img _ ->
            ()
        | X f -> Option.iter (List.iter self#omd) (f#to_t [])

      method type_ (_ : Type.t) = ()

      method see { see_reference; see_label = _; see_description } =
        self#reference see_reference;
        Option.iter self#description see_description

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
    end
end

module Lift (L : S) (R : S) = struct
  type t =
    { any_ref : L.reference -> R.reference;
      type_ref : L.reference -> R.reference;
      description : L.description -> R.description
    }

  let description x = x.description

  let example lift : L.example -> R.example = function
    | RawExample e -> RawExample e
    | RichExample d -> RichExample (description lift d)

  let see lift { L.see_reference; see_label; see_description } =
    { R.see_reference = lift.any_ref see_reference;
      see_label;
      see_description = Option.map (description lift) see_description
    }

  module Ty_lift = Type_syntax.Lift (L.Type) (R.Type)

  let ty x = Ty_lift.t x.type_ref

  let arg lift { L.arg_name; arg_opt; arg_type; arg_description } =
    { R.arg_name;
      arg_opt;
      arg_type = Option.map (ty lift) arg_type;
      arg_description = Option.map (description lift) arg_description
    }

  let return lift { L.ret_type; ret_many; ret_description } =
    { R.ret_type = Option.map (ty lift) ret_type;
      ret_many;
      ret_description = Option.map (description lift) ret_description
    }
end
