open IlluaminateCore
open IlluaminateConfig

type custom_kind =
  { id : string;  (** The id of this kind, used in documentation tags. *)
    display : string  (** A display name for this kind. *)
  }

(** Configuration options for documentation querying. *)
type t =
  { module_path : IlluaminatePattern.t list;
        (** The path(s) where modules are located as globs. For instance, the Lua path component
            [\[ foo/?.lua\]] would be encoded as [\["/path/to/project/foo/*.lua"\]].

            This path is used for guessing the module name of a file. It is ignored when an explicit
            [\[@module\]] annotation is provided. *)
    module_kinds : custom_kind list  (** All custom module kinds. *)
  }

(** A config category for all workspace-level documentation options *)
val workspace : Category.t

(** A configuration key for {!t}. *)
val key : t Category.key

(** Guess the module name from a file path. *)
val guess_module : Fpath.t -> IlluaminateData.Programs.Context.t -> string option

(** Guess the module name from a file name. *)
val guess_module' : Span.filename -> IlluaminateData.context -> string option
