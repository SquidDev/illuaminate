(** Extract documentation comments from a program and resolve them. *)

open Doc_syntax

module Config : sig
  open IlluaminateConfig

  type custom_kind =
    { id : string;  (** The id of this kind, used in documentation tags. *)
      display : string  (** A display name for this kind. *)
    }

  (** Configuration options for documentation querying. *)
  type t =
    { module_path : IlluaminatePattern.t list;
          (** The path(s) where modules are located as globs. For instance, the Lua path component
              [[ foo/?.lua]] would be encoded as [["/path/to/project/foo/*.lua"]].

              This path is used for guessing the module name of a file. It is ignored when an
              explicit [[@module]] annotation is provided. *)
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
val file : t IlluaminateData.Programs.key

(** An error that occurred during doc extraction. *)
module Extract_error : sig
  type t =
    | Value_mismatch of IlluaminateCore.Span.t * value * value
    | Func_and_type of IlluaminateCore.Span.t
    | Func_and_field of IlluaminateCore.Span.t

  (** Convert this error to a {!Illuaminate.Error.t}. *)
  val to_error : t -> Illuaminate.Error.t
end

(** Get any errors which occurred as part of documentation extraction. These errors are guaranteed
    to have a tag in {!Tags.all} *)
val errors : t -> Extract_error.t list

(** Comments which were not attached to any node, and thus not processed. *)
val detached_comments : t -> Doc_comment.comment list

(** Get the documented node for this variable. *)
val get_var : t -> Resolve.var -> value documented option

(** Get the documented module for this program. *)
val get_page : t -> page documented option

(** Get all available pages. *)
val all_pages :
  (unit, page documented Map.Make(String).t Map.Make(Namespace).t) IlluaminateData.Key.t

(** Get all public (i.e. non-[@local]) pages. *)
val public_pages :
  (unit, page documented Map.Make(String).t Map.Make(Namespace).t) IlluaminateData.Key.t
