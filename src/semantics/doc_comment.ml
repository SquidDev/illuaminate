open IlluaminateCore

type reference = Reference.unresolved = Reference of string [@@unboxed]

include Doc_abstract_syntax.Make (struct
  type nonrec reference = reference

  module Type = Type_syntax.Unresolved
end)

type module_info =
  { mod_name : string;
    mod_namespace : Namespace.t option;
    mod_kind : Doc_syntax.module_kind option
  }

type type_info = { type_name : string } [@@unboxed]

type field =
  { field_pos : Span.t;
    field_name : string;
    field_type : Type.t option;
    field_description : description option
  }

type comment =
  { (* Some general information about this comment. *)
    source : Span.t;
    errors : (Error.Tag.t * Span.t * string) list;
    (* Shared fields across all types. *)
    description : description option;
    see : see list;
    examples : example list;
    local : bool;
    includes : reference Span.spanned list;
    export : bool;
    deprecated : deprecation option;
    custom_source : position option;
    changes : changes;
    (* Functions. *)
    arguments : arg list list;
    returns : return list list;
    throws : description list;
    (* Tables *)
    fields : field list;
    (* Modules. *)
    module_info : module_info Span.spanned option;
    (* Types. *)
    type_info : type_info option
  }
