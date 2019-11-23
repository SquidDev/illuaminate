(** Handles resolving variables and doing a basic dataflow analysis. *)

open IlluaminateCore

(** The scope of this variable. *)
type scope = private
  { scope_id : int;
    fun_id : int
  }

type function_scope = scope

(** The kind of this variable. *)
type kind = private
  | Global
  | Arg of function_scope  (** An argument to a function. *)
  | ImplicitArg of function_scope  (** The implicit "arg" table. *)
  | Local of scope  (** A variable captured in a bound variable. *)
  | Loop of scope  (** A loop bound variable. *)

(** Where this variable is defined. *)
type definition =
  | Declare
      (** This variable is unbound (in a {!local}) if a local, or unknown (if an argument or loop
          variable) *)
  | OfExpr of Syntax.expr  (** This variable is bound to an expression. *)
  | OfSelect of int * Syntax.expr
      (** This variable is bound to the nth part of a varargs (where n is 0 indexed, and should be >
          0). *)
  | OfFunction of Syntax.args * Syntax.block

(** The canonical source of a variable. *)
type var = private
  { name : string;  (** The name of this variable. *)
    kind : kind;  (** The kind of this variable. *)
    shadows : var option;  (** The variable we shadow. *)
    mutable usages : var_usage list;
    mutable definitions : definition list;  (** The list of all definitions. *)
    mutable captured : bool;  (** Whether this variable is captured in a closure. *)
    mutable upvalue_mutated : bool  (** Whether this variable is mutated in a closure. *)
  }

(** The usage of a variable. *)
and var_usage = private
  { var : var;  (** The variable which is bound. *)
    node : Syntax.var
  }

type dots = private
  { dot_scope : function_scope;  (** The function scope with which this dots is bound. *)
    dot_node : Syntax.token option;
        (** The argument which defines this dots. {!None} when this is the top-level function. *)
    dot_implicit : var option;  (** The implicit "arg" variable introduced by this dots. *)
    mutable dot_usages : dots_usage list
  }

(** The usage of a {!Syntax.Dots} argument ([...]) *)
and dots_usage = private
  | IllegalDots  (** Using dots outside of a function which declares it. *)
  | BoundDots of
      { dots : dots;
        node : Syntax.token
      }

(** A hash table of variables. This may be used as a convenient store for variable information. *)
module VarTbl : Hashtbl.S with type key = var

(** The primary store of resolution information for a program. *)
type t

val key : t IlluaminateCore.Data.key

(** Get the definition of a variable.

    Note, this {i must} be used on the definition site of a variable (local, argument, etc...). We
    will throw if not. *)
val get_definition : Syntax.var -> t -> var

(** Get the usage of a variable.

    Note, this {i must} be used on a variable expression. *)
val get_usage : Syntax.var -> t -> var_usage

(** Get the definition of an dots argument. *)
val get_dots : Syntax.token -> t -> dots

(** Get the usage of a dots argument. *)
val get_dots_usage : Syntax.token -> t -> dots_usage
