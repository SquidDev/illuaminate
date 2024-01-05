open IlluaminateCore

type reference = Reference.unresolved = Reference of string [@@unboxed]

include Doc_abstract_syntax.Make (struct
  type nonrec reference = reference

  module Type = Type_syntax.Unresolved
end)

module Comment_error = struct
  type t =
    | Unknown_tag of
        { span : Span.t;
          tag : string
        }
    | Unknown_flag of
        { span : Span.t;
          tag : string;
          flag : string
        }
    | Comment_error of
        { code : string;
          span : Span.t;
          message : string
        }

  let severity = Illuaminate.Error.Error

  let to_error : t -> Illuaminate.Error.t = function
    | Unknown_tag { span; tag } ->
        Illuaminate.Error.simple ~code:"doc:unknown-tag" ~severity (Span.to_error_position span)
          (fun f -> f "Unknown tag @%s" tag)
    | Unknown_flag { span; tag; flag } ->
        Illuaminate.Error.simple ~code:"doc:unknown-flag" ~severity (Span.to_error_position span)
          (fun f -> f "%s has unknown flag '%s'" tag flag)
    | Comment_error { code; span; message } ->
        Illuaminate.Error.simple ~code ~severity (Span.to_error_position span) (fun f ->
            f "%s" message)
end

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
    errors : Comment_error.t list;
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
