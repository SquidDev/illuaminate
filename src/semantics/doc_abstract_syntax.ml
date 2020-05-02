open IlluaminateCore

type module_kind =
  | Module
  | Library

module Omd' = struct
  open Omd

  let iter_element f =
    let rec go x =
      f x;
      match x with
      | H1 x | H2 x | H3 x | H4 x | H5 x | H6 x | Paragraph x | Emph x | Bold x -> List.iter go x
      | Url (_, x, _) | Blockquote x | Html (_, _, x) | Html_block (_, _, x) -> List.iter go x
      | Ul xs | Ol xs | Ulp xs | Olp xs -> List.iter (List.iter go) xs
      | Ref (_, _, _, f) | Img_ref (_, _, _, f) -> List.iter go f#to_t
      | Text _ | Code _ | Code_block _ | Br | Hr | NL | Html_comment _ | Raw _ | Raw_block _ | Img _
        ->
          ()
      | X f -> Option.iter (List.iter go) (f#to_t [])
    in
    go

  let iter f = List.iter (iter_element f)
end

module type S = sig
  type reference

  module Type : Type_syntax.S with type reference = reference

  type description =
    { description : Omd.t;
      description_pos : Span.t option
    }

  (** A link to a string, within a {!description}. *)
  type link =
    { link_reference : reference;
      link_label : description;
      link_style : [ `Text | `Code ]
    }

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

      method type_ : Type.t -> unit

      method see : see -> unit

      method deprecation : deprecation -> unit

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

  type description =
    { description : Omd.t;
      description_pos : Span.t option
    }

  type link =
    { link_reference : reference;
      link_label : description;
      link_style : [ `Text | `Code ]
    }

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
