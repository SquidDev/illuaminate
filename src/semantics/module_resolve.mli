(** Resolves variables and names, resolving pointers to names within other modules. *)

open IlluaminateCore
open Reference
open Doc_syntax

(** The primary store of resolution information for a program. *)
type t

val key : t Data.key

(** Get the reference this variable is pointing to. This is guaranteed to never be {!Unknown}. *)
val get_var : t -> Syntax.var -> (resolved * value documented) option

(** Get the reference this name is pointing to. This is guaranteed to never be {!Unknown}. *)
val get_name : t -> Syntax.name -> (resolved * value documented) option
