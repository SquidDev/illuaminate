(** A reference to some name in a module.

    Unlike {!Resolve}, the names here don't necessarily have any correspondence to variable names
    within Lua source code. *)

open IlluaminateCore

(** An unresolved variable name. *)
type unresolved = Reference of string [@@unboxed]

(** Print an {!unresolved} reference. *)
let pp_unresolved out (Reference name) = Format.pp_print_string out name

(** The kind of term this value points to. *)
type name_of =
  | Module
  | Value of string
  | Type of string
  | Member of string * string

(** A name which has been resolved to a known location. *)
type resolved =
  | Internal of
      { in_module : Namespace.Ref.t;  (** The page/module which this reference belongs to. *)
        name : name_of;  (** The name within this module. *)
        definition : Span.t  (** The location of this definition. *)
      }  (** A reference to somewhere within this compilation unit. *)
  | External of
      { name : string;  (** The name of this resolved reference. *)
        url : string option  (** An optional URL this name points to. *)
      }  (** A reference to a name in another library (e.g. the Lua standard library). *)
  | Unknown of string  (** A reference which cannot be resolved. *)

(** Print an {!resolved} reference. *)
let pp_resolved out = function
  | Internal { in_module; name = Module; _ } ->
      Format.pp_print_string out (Namespace.Ref.display_name in_module)
  | Internal { in_module = { id = in_module; _ }; name = Value n | Type n; _ } ->
      Format.fprintf out "%s.%s" in_module n
  | Internal { in_module = { id = in_module; _ }; name = Member (ty, n); _ } ->
      Format.fprintf out "%s.%s:%s" in_module ty n
  | External { name; _ } | Unknown name -> Format.pp_print_string out name

(** Get the name of a section for a specific reference. *)
let section_of_name = function
  | Module -> None
  | Value n -> Printf.sprintf "v:%s" n |> Option.some
  | Type n -> Printf.sprintf "ty:%s" n |> Option.some
  | Member (ty, n) -> Printf.sprintf "ty:%s:%s" ty n |> Option.some
