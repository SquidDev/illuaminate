(** Resolves variables and names, resolving pointers to names within other modules. *)

open IlluaminateCore
open Doc_syntax

(** The primary store of resolution information for a program. *)
type t

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
