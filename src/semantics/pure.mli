(** Various tools for determining the safety and side effects of various terms. *)

open IlluaminateCore.Syntax

(** A "safe" term is such a expression which is side effect free, and thus can be removed without
    impacting evaluation.

    This is subtlly different to purity - a side-effect free term may return different results each
    time it is evaluated. *)
module Safe : sig
  (** Determine if an expression is side-effect free. *)
  val expr : expr -> bool

  (** Determine if a name is side-effect free. *)
  val name : name -> bool

  (** Determine if a table is side-effect free. *)
  val table : table -> bool
end
