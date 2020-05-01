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
  | Arg of
      { scope : function_scope;
        def : Syntax.var
      }  (** An argument to a function. *)
  | ImplicitArg of
      { scope : function_scope;
        kind : [ `Self | `Arg ];  (** The name of this implicit argument. *)
        def : Syntax.args  (** The arguments where this function was introduced. *)
      }  (** An implicitly introduced argument. *)
  | Local of
      { scope : scope;
        def : Syntax.var
      }  (** A variable captured in a bound variable. *)
  | Loop of
      { scope : scope;
        def : Syntax.var
      }  (** A loop bound variable. *)

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
    mutable definitions : (var_usage option * definition) list;
        (** The list of all definitions. If this is {!None}, this is a "magic" variable, such as
            [self] or [arg]. *)
    mutable captured : bool;  (** Whether this variable is captured in a closure. *)
    mutable upvalue_mutated : bool  (** Whether this variable is mutated in a closure. *)
  }

(** A location a variable is used. Unlike the {!recfield:usages} field, this corresponds to both
    assignments and definitions. *)
and var_usage = private
  { var : var;  (** The variable which is bound. *)
    node : Syntax.var;  (** The node where this variable is used. *)
    snapshot : var Map.Make(String).t  (** A snapshot of the current scope. *)
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

val key : t IlluaminateData.Programs.key

(** Get the definition of a variable.

    Note, this {i must} be used on the definition site of a variable (local, argument, etc...). We
    will throw if not. *)
val get_definition : Syntax.var -> t -> var

(** Get the usage of a variable.

    Note, this {i must} be used on a variable expression. *)
val get_usage : Syntax.var -> t -> var_usage

(** Get the underlying variable. This may be used on usages or definitions. *)
val get_var : Syntax.var -> t -> var

(** Get the definition of an dots argument. *)
val get_dots_definition : Syntax.token -> t -> dots

(** Get the usage of a dots argument. *)
val get_dots_usage : Syntax.token -> t -> dots_usage

(** Get the underlying dots instance. *)
val get_dots : Syntax.token -> t -> dots option

(** Get all globals, either used or defined. *)
val globals : t -> var Seq.t

(** Look up a global if it was used within this program. *)
val get_global : t -> string -> var option
