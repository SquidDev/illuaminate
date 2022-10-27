open IlluaminateCore

type reference = Reference.resolved

include
  Doc_abstract_syntax.S with type reference := reference and module Type = Type_syntax.Resolved

(** Some abstract documented object. *)
type 'a documented =
  { description : description option;  (** The description of this documented node. *)
    descriptor : 'a;  (** The type of this documented node *)
    definition : Span.t;  (** The location this node was defined at *)
    examples : example list;  (** One or more examples *)
    see : see list;  (** List of references to other symbols. *)
    local : bool;  (** If this definition should not be exposed. *)
    export : bool;  (** Whether this term is the root exported term. *)
    deprecated : deprecation option;  (** Whether this term is deprecated. *)
    custom_source : position option;  (** Override the position of this term. *)
    changes : changes  (** Historic changes to this term. *)
  }

(** A value or expression which may be used in a documented program. *)
type value =
  | Function of
      { args : arg list list;  (** The arguments to this function. *)
        rets : return list list;  (** The return values of this function. *)
        throws : description list;  (** The reasons this function may throw. *)
        has_self : bool  (** If this function has an {i implicit} self. *)
      }
  | Table of (string * value documented) list  (** An ordered set of fields this table declares. *)
  | Expr of
      { ty : Type.t;  (** The type of this expression. *)
        value : string option  (** The string representation of this value. *)
      }
  | Type of type_info
  | Unknown
      (** An value with an unknown type. This is effectively the top type, in that any
          specialisation will always narrow away from the unknown type. *)
  | Undefined
      (** A value with an undefined type. This is effectively the top type, in that any
          specialisation narrows towards it. Any invalid specialisation will be replaced with this,
          preventing further potential errors. *)

(** A member belonging to a documented {!type_info}. *)
and member =
  { member_name : string;
        (** The name of this member. Like the fields in {!Table}, this will be a Lua identifier. *)
    member_is_method : bool;
        (** Whether this member is a method, and so should be printed as [Type:foo] instead. *)
    member_value : value documented  (** The value of this member. *)
  }

(** A documented type. *)
and type_info =
  { type_name : string;  (** The name of this type. *)
    type_members : member list  (** The members belonging to this type. *)
  }

type page =
  { page_ref : Namespace.Ref.t;
        (** The unique reference of this page, including id, namespace and an optional title. *)
    page_value : value option;
        (** The value this module exposes. This may be {!None} if this module is not directly
            consumable in user code. *)
    page_types : type_info documented list;  (** Types defined in this page. *)
    page_module_kind : module_kind  (** The kind of this module. *)
  }

(** Get additional information about a definition, to be appended after a name.

    This is only intended to be a brief summary of the definition (such as its signature or value),
    not a complete description. *)
val get_suffix : value -> string

(** Determine if a documented term actually has a doc comment. *)
val is_documented : 'a documented -> bool

(** A base class for visitors over the document syntax tree. *)
class iter :
  object
    method abstract_syntax : span:Span.t -> abstract_iter
    method documented : 'a. (span:Span.t -> 'a -> unit) -> 'a documented -> unit
    method value : span:Span.t -> value -> unit
    method member : member -> unit
    method type_info : span:Span.t -> type_info -> unit
    method page : span:Span.t -> page -> unit
  end

val iter_of : (span:Span.t -> abstract_iter) -> iter
