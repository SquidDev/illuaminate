(** A reference to some name in a module.

    Unlike {!Resolve}, the names here don't necessarily have any correspondence to variable names
    within Lua source code. *)

(** An unresolved variable name. *)
type unresolved = Reference of string [@@unboxed]

(** Print an {!unresolved} reference. *)
let pp_unresolved out (Reference name) = Format.pp_print_string out name

(** A name which has been resolved to a known location. *)
type resolved =
  | Internal of
      { in_module : string;  (** The module which this reference belongs to. *)
        name : string option  (** The name within this module. *)
      }  (** A reference to somewhere within this compilation unit. *)
  | External of
      { name : string;  (** The name of this resolved reference. *)
        url : string option  (** An optional URL this name points to. *)
      }  (** A reference to a name in another library (e.g. the Lua standard library). *)
  | Unknown of string  (** A reference which cannot be resolved. *)

(** Print an {!resolved} reference. *)
let pp_resolved out = function
  | Internal { in_module; name = None } -> Format.pp_print_string out in_module
  | Internal { in_module; name = Some n } -> Format.fprintf out "%s.%s" in_module n
  | External { name; _ } | Unknown name -> Format.pp_print_string out name
