(** Resolves variables and names, resolving pointers to names within other modules. *)

open IlluaminateCore
open Doc_syntax

(** The primary store of resolution information for a program. *)
type t

(** Compute the map of resolved variables.

    This should only be used for free-standing snippets of code. {!key} should be used when dealing
    with existing programs. *)
val compute : data:IlluaminateData.context -> resolved:Resolve.t -> ?doc:Doc_extract.t -> unit -> t

val key : t IlluaminateData.Programs.key

module Reference : sig
  (** A reference to a name, possibly in another module. *)
  type t =
    | Reference of Reference.resolved
    | Var of Resolve.var
    | Dot of t * string

  (** Remove all indexes into this node, just returning the root node. *)
  val root : t -> [ `Reference of Reference.resolved | `Var of Resolve.var ]

  val pp : Format.formatter -> t -> unit
end

(** Get the reference this variable is pointing to. *)
val get_var : t -> Syntax.var -> (Reference.t * value documented) option

(** Get the reference this name is pointing to. *)
val get_name : t -> Syntax.name -> (Reference.t * value documented) option

(** Determine if a documented node is "interesting". Something is interesting, if it comes from
    another module or it has a doc comment. *)
val is_interesting : Reference.t -> value documented -> bool

(** The name of all global modules. *)
val global_modules : (unit, Set.Make(String).t) IlluaminateData.Key.t
