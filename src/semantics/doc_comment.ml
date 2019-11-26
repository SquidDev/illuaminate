open IlluaminateCore

type reference = Reference.unresolved = Reference of string [@@unboxed]

include Doc_abstract_syntax.Make (struct
  type nonrec reference = reference

  module Type = Type_syntax.Unresolved
end)

type module_info =
  { mod_name : string;
    mod_kind : module_kind
  }

type type_info = { type_name : string } [@@unboxed]

type comment =
  { (* Some general information about this comment. *)
    source : Span.t;
    errors : (Error.Tag.t * string) list;
    (* Shared fields across all types. *)
    description : description option;
    see : see list;
    examples : example list;
    local : bool;
    includes : reference list;
    (* Functions. *)
    arguments : arg list list;
    returns : return list list;
    throws : description list;
    (* Modules. *)
    module_info : module_info option;
    (* Types. *)
    type_info : type_info option
  }
