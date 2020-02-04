open IlluaminateCore

type reference = Reference.unresolved = Reference of string [@@unboxed]

include
  Doc_abstract_syntax.S with type reference := reference and module Type = Type_syntax.Unresolved

(** Information about this module. *)
type module_info = { mod_name : string  (** The name of this module. *) } [@@unboxed]

(** Information about this type/class. *)
type type_info = { type_name : string } [@@unboxed]

(** A documentation comment.

    This is one behemoth of a documentation comment, as it includes all possible fields a doc
    comment can take, without fully validating it. *)
type comment =
  { (* Some general information about this comment. *)
    source : Span.t;  (** Where this doc comment originates from. *)
    errors : (Error.Tag.t * string) list;  (** Additional errors from this term. *)
    (* Shared fields across all types. *)
    description : description option;  (** The description for this documented term. *)
    see : see list;  (** All [@see] tags. *)
    examples : example list;  (** All [@usage] tags. *)
    local : bool;  (** Whether this term was tagged as [@local], and so should not be exported. *)
    includes : reference list;  (** Other terms to include. *)
    (* Functions. *)
    arguments : arg list list;  (** A list of possible function argument signatures. *)
    returns : return list list;  (** A list of possible function return signatures. *)
    throws : description list;  (** Possible errors this function throws. *)
    (* Modules. *)
    module_info : module_info option;  (** Information about the module this term defines. *)
    (* Types. *)
    type_info : type_info option  (** Information about the type this term defines. *)
  }
