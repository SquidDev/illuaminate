open IlluaminateCore

type reference = Reference.unresolved = Reference of string [@@unboxed]

include
  Doc_abstract_syntax.S with type reference := reference and module Type = Type_syntax.Unresolved

(** A comment that occurs when parsing a doc comment. *)
module Comment_error : sig
  type t =
    | Unknown_tag of
        { span : Span.t;  (** The position of this tag. *)
          tag : string  (** The name of the tag. *)
        }  (** An unknown tag was encountered, for instance [@foo]. *)
    | Unknown_flag of
        { span : Span.t;  (** The position of this flag. *)
          tag : string;  (** The tag this flag appears under. *)
          flag : string  (** The name of the unknown flag. *)
        }  (** An unknown flag. *)
    | Comment_error of
        { code : string;
          span : Span.t;
          message : string
        }

  (** Convert this error to a {!Illuaminate.Error.t}. *)
  val to_error : t -> Illuaminate.Error.t
end

(** Information about this module. *)
type module_info =
  { mod_name : string;  (** The name of this module. *)
    mod_namespace : Namespace.t option;  (** This namespace of this module. *)
    mod_kind : module_kind option  (** This kind of this module. *)
  }

(** Information about this type/class. *)
type type_info = { type_name : string } [@@unboxed]

(** An additional field on a type, not described in the Lua code itself. *)
type field =
  { field_pos : Span.t;  (** The position of this field. *)
    field_name : string;  (** The name of this field. *)
    field_type : Type.t option;  (** The type of this field. *)
    field_description : description option  (** An additional description of the field. *)
  }

(** A documentation comment.

    This is one behemoth of a documentation comment, as it includes all possible fields a doc
    comment can take, without fully validating it. *)
type comment =
  { (* Some general information about this comment. *)
    source : Span.t;  (** Where this doc comment originates from. *)
    errors : Comment_error.t list;  (** Additional errors from this term. *)
    (* Shared fields across all types. *)
    description : description option;  (** The description for this documented term. *)
    see : see list;  (** All [@see] tags. *)
    examples : example list;  (** All [@usage] tags. *)
    local : bool;  (** Whether this term was tagged as [@local], and so should not be exported. *)
    includes : reference Span.spanned list;  (** Other terms to include. *)
    export : bool;  (** Whether this term is the "root" of this documentation node. *)
    deprecated : deprecation option;  (** Whether this term is deprecated. *)
    custom_source : position option;  (** Override the position of this term. *)
    changes : changes;  (** Changes about this term .*)
    (* Functions. *)
    arguments : arg list list;  (** A list of possible function argument signatures. *)
    returns : return list list;  (** A list of possible function return signatures. *)
    throws : description list;  (** Possible errors this function throws. *)
    (* Tables *)
    fields : field list;  (** Additional fields on this type. *)
    (* Modules. *)
    module_info : module_info Span.spanned option;
        (** Information about the module this term defines. *)
    (* Types. *)
    type_info : type_info option  (** Information about the type this term defines. *)
  }
