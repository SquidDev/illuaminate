open IlluaminateCore

(** The kind of a module. This describes how it is defined and loaded. *)
type module_kind =
  | MKModule
      (** A legacy module, using the "module" directive. Global variables declared in this file are
          considered as exported by this file. *)
  | MKLibrary
      (** A standard module, loaded using [require] which returns the term that it exports. *)
  | MKNone
      (** This module has no custom behaviour. It may be referenced through doc-comments, but does
          not defined variables that are directly usable in Lua code. *)

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

(** The kind of change in a changelog. *)
type change_kind =
  | Added  (** This feature was introduced in a specific version. *)
  | Changed  (** This feature was changed in a specific version. *)

(** Meta keys for various Cmarkit extensions. *)
module Cmarkit_meta : sig
  (** Admonition level for GitHub-style admonition labels (e.g. [[DANGER]]) *)
  val admonition_level : Cmarkit.Block.Admonition.level Cmarkit.Meta.key
end

(** Converts positions in a markdown fragment to those in a Lua source file. *)
module Comment_lines : sig
  type t

  (** Internal function to construct this map. DO NOT USE *)
  val __make : Span.t Map.Make(Int).t -> t

  (** The total range of this markdown fragment. *)
  val span : t -> Span.t

  (** Create a span from two text locations. *)
  val span_of_range : t -> Cmarkit.Textloc.byte_pos -> Cmarkit.Textloc.byte_pos -> Span.t

  (** Create a span from a text location. Returns {!None} if the location is
      {!Cmarkit.Textloc.is_none is none}. *)
  val span_of_textloc : t -> Cmarkit.Textloc.t -> Span.t option

  (** Create a span from a meta's text location. Returns {!None} if the location is
      {!Cmarkit.Textloc.is_none is none}. *)
  val span_of_meta : t -> Cmarkit.Meta.t -> Span.t option

  (** Create a span from a node's location. Returns {!None} if the location is
      {!Cmarkit.Textloc.is_none is none}. *)
  val span_of_node : t -> 'a Cmarkit.node -> Span.t option
end

module type S = sig
  type reference

  module Type : Type_syntax.S with type reference = reference

  module Markdown : sig
    type t = Markdown of Cmarkit.Doc.t [@@unboxed]

    (** Meta key for labels containing a reference to a valid identifier. *)
    val reference : reference Cmarkit.Meta.key

    (** Extract the underlying document from this Markdown fragment *)
    val doc : t -> Cmarkit.Doc.t

    (** Attempt to convert this fragment into a single paragraph. *)
    val as_single_paragraph : t -> Cmarkit.Inline.t option

    (** Iterate over references in this fragment. *)
    val iter_references : (Cmarkit.Label.t -> reference -> unit) -> t -> unit
  end

  type description =
    { description : Markdown.t;
      description_pos : Comment_lines.t
    }

  type nonrec module_kind = module_kind =
    | MKModule
    | MKLibrary
    | MKNone

  type nonrec change_kind = change_kind =
    | Added
    | Changed

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

  type nonrec position = position =
    { path : string;
      start_line : int;
      end_line : int
    }

  (** A change which occurred to this documented term. *)
  type change =
    { change_kind : change_kind;  (** The kind of this change. *)
      change_version : string;  (** The version this change occurred in *)
      change_span : Span.t;  (** The position of this change entry. *)
      change_description : description option
    }

  (** An ordered list of changes to a term, from oldest to most recent. *)
  type changes = change list

  (** A base class for visitors over the document syntax tree. *)
  class abstract_iter : object
    method reference : reference -> unit
    method description : description -> unit

    (** Visit a type. Note, by default this does not visit any references within this type. *)
    method type_ : Type.t -> unit

    method see : see -> unit
    method deprecation : deprecation -> unit
    method example : example -> unit
    method arg : arg -> unit
    method return : return -> unit
    method change : change -> unit
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
    { any_ref : L.reference -> string option * R.reference;  (** Lift any kind of reference. *)
      type_ref : L.reference -> R.reference  (** Lift references specialised for types. *)
    }

  val markdown : t -> L.Markdown.t -> R.Markdown.t
  val description : t -> L.description -> R.description
  val see : t -> L.see -> R.see
  val deprecation : t -> L.deprecation -> R.deprecation
  val example : t -> L.example -> R.example
  val arg : t -> L.arg -> R.arg
  val return : t -> L.return -> R.return
  val change : t -> L.change -> R.change
  val ty : t -> L.Type.t -> R.Type.t
end
