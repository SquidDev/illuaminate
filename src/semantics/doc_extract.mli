(** Extract documentation comments from a program and resolve them. *)

open IlluaminateCore
open Doc_syntax
module StringMap := Map.Make(String)

module Tag : sig
  (** All errors which may be reported as part of documentation extraction.

      Errors with these tags can be extracted by {!errors}. *)
  val all : Error.Tag.t list
end

module Config : sig
  open IlluaminateConfig

  type custom_kind =
    { id : string;  (** The id of this kind, used in documentation tags. *)
      display : string  (** A display name for this kind. *)
    }

  (** Configuration options for documentation querying. *)
  type t =
    { module_path : (string * string) list;
          (** The path(s) where modules are located. Each item is a tuple of an (absolute) file path
              prefix, and a suffix (normally an extension). For instance, the Lua path component
              [\[ foo/?.lua\]] would be encoded as [\["/path/to/project/foo/", ".lua"\]].

              This path is used for guessing the module name of a file. It is ignored when an
              explicit [\[@module\]] annotation is provided. *)
      module_kinds : custom_kind list  (** All custom module kinds. *)
    }

  (** A config category for all workspace-level documentation options *)
  val workspace : Category.t

  (** A configuration key for {!t}. *)
  val key : t Category.key
end

(** Information about a program's doc comments. *)
type t

(** The key to query the data cache with. *)
val key : t IlluaminateData.Programs.key

(** Get any errors which occurred as part of documentation extraction. These errors are guaranteed
    to have a tag in {!Tags.all} *)
val errors : t -> Error.Error.t list

(** Comments which were not attached to any node, and thus not processed. *)
val detached_comments : t -> Doc_comment.comment list

(** Get the documented node for this variable. *)
val get_var : t -> Resolve.var -> value documented option

(** Get the documented module for this program. *)
val get_module : t -> module_info documented option

(** Get all available modules. *)
val get_modules : (unit, module_info documented Map.Make(String).t) IlluaminateData.Key.t
