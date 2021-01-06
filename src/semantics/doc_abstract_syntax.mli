open IlluaminateCore

(** The "kind" of a Lua module. This controls how it is loaded and exposes members. *)
type module_kind =
  | Module
      (** A legacy module, using the "module" directive. Global variables declared in this file are
          considered as exported by this file. *)
  | Library  (** A standard module, which returns the term that it exports. *)
  | Custom of string  (** A custom module kind. *)

(** A position within a file. This may be used instead of {!Span.t} for files not opened by
    illuaminate. *)
type position =
  { path : string;
    start_line : int;
    end_line : int
  }

(** A source of a node. Either a {!position} or {!Span.t}. *)
type source =
  | Span of Span.t
  | Position of position

module Omd' : sig
  val iter_element : (Omd.element -> unit) -> Omd.element -> unit

  val iter : (Omd.element -> unit) -> Omd.t -> unit
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
    { link_reference : reference;  (** The name this link points to. *)
      link_label : description;  (** The description of this link. *)
      link_style : [ `Text | `Code ]  (** Whether this label can be considered as text or code. *)
    }

  (** A link to another name. *)
  type see =
    { see_reference : reference;  (** The name this see link points to. *)
      see_label : string;  (** Textual representation of the reference. *)
      see_span : Span.t;  (** The position of the label. *)
      see_description : description option  (** An optional description of this name. *)
    }

  (** This term has been deprecated. *)
  type deprecation =
    { deprecation_message : description option  (** The reason this term has been deprecated. *) }
  [@@unboxed]

  (** An example of how to use this code. *)
  type example =
    | RawExample of string Span.spanned  (** An example with no associated description. *)
    | RichExample of description  (** A rich markdown block, with some associated comments. *)

  type opt_arg =
    | Required  (** This argument is required. *)
    | Optional  (** This argument is optional. *)
    | Default of string  (** This argument is required with a default value. *)

  (** An argument to a function. *)
  type arg =
    { arg_name : string;  (** The argument's name. *)
      arg_opt : opt_arg;  (** Whether the argument is optional or not. *)
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
        (** A legacy module, using the "module" directive. Global variables declared in this file
            are considered as exported by this file. *)
    | Library  (** A standard module, which returns the term that it exports. *)
    | Custom of string  (** A custom module kind. *)

  type nonrec position = position =
    { path : string;
      start_line : int;
      end_line : int
    }

  (** A base class for visitors over the document syntax tree. *)
  class abstract_iter :
    object
      method reference : reference -> unit

      method description : description -> unit

      (** Visit a type. Note, by default this does not visit any references within this type. *)
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
end) : S with type reference := X.reference and module Type = X.Type

(** Lift a type from using one reference to using another. *)
module Lift (L : S) (R : S) : sig
  (** A series of functions to map between one reference kind and another. *)
  type t =
    { any_ref : L.reference -> R.reference;  (** Lift any kind of reference. *)
      type_ref : L.reference -> R.reference;  (** Lift references specialised for types. *)
      description : L.description -> R.description
          (** Lift a description. This must handle [\[illuaminate:ref\]] HTML tags. *)
    }

  val description : t -> L.description -> R.description

  val see : t -> L.see -> R.see

  val deprecation : t -> L.deprecation -> R.deprecation

  val example : t -> L.example -> R.example

  val arg : t -> L.arg -> R.arg

  val return : t -> L.return -> R.return

  val ty : t -> L.Type.t -> R.Type.t
end
