open IlluaminateCore

type reference = Reference.unresolved = Reference of string [@@unboxed]

include Doc_abstract_syntax.Make (struct
  type nonrec reference = reference

  module Type = Type_syntax.Unresolved
end)

type module_info =
  { mod_name : string;
    mod_kind : module_kind option
  }

type type_info = { type_name : string } [@@unboxed]

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
    (* Functions. *)
    arguments : arg list list;
    returns : return list list;
    throws : description list;
    (* Modules. *)
    module_info : module_info option;
    (* Types. *)
    type_info : type_info option
  }

module Link = struct
  let of_tag tags description =
    match tags with
    | [ ("link", Some link); ("style", Some style) ] ->
        let style =
          match style with
          | "text" -> `Text
          | "code" -> `Code
          | _ -> invalid_arg "Invalid style"
        in
        { link_reference = Reference link;
          link_label = { description; description_pos = None };
          link_style = style
        }
    | _ -> invalid_arg "malformed illuaminate:ref"

  let to_tag { link_reference = Reference link; link_label = { description; _ }; link_style } :
      Omd.element =
    let style =
      match link_style with
      | `Code -> "code"
      | `Text -> "text"
    in
    Html ("illuaminate:ref", [ ("link", Some link); ("style", Some style) ], description)
end
