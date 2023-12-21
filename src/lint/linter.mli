(** The base type for linters, and the information they produce. *)

open Illuaminate
open IlluaminateCore

module Fixer : sig
  (** A fixer for a problematic node. This should map a term of some type, to an equivalent
      no-problematic term. *)
  type 'a t = private
    | Nothing : 'a t  (** This node has no fixer. *)
    | One : ('a -> ('a, string) result) -> 'a t
        (** This node can be fixed by replacing it with a single equivalent node. If the node cannot
            be fixed, we return {!Error}. *)
    | Block : (Syntax.stmt -> (Syntax.stmt list, string) result) -> Syntax.stmt t
        (** This statement can be fixed by replacing it with 0 or more statements. *)

  (** The fixer which does nothing. See {!Nothing} *)
  val none : 'a t

  (** Construct a fixer which replaces it with an equivalent node. See {!One}. *)
  val fix : ('a -> ('a, string) result) -> 'a t

  (** Construct a fixer which replaces a statement with a (possibly empty) list of statements. See
      {!Block} *)
  val block : (Syntax.stmt -> (Syntax.stmt list, string) result) -> Syntax.stmt t
end

(** A reporter, which may be used to report notes to the user. *)
type 'a reporter =
  { r :
      'f.
      ?fix:'a Fixer.t ->
      ?span:Span.t ->
      ?detail:(Format.formatter -> unit) ->
      tag:Error.Tag.t ->
      ('f, Format.formatter, unit, unit) format4 ->
      'f;
        (** Report a problem at the current location, with an optional fixer. *)
    e :
      'a 'f.
      ?fix:'a Fixer.t ->
      ?span:Span.t ->
      ?detail:(Format.formatter -> unit) ->
      tag:Error.Tag.t ->
      kind:'a Witness.t ->
      source:'a ->
      ('f, Format.formatter, unit, unit) format4 ->
      'f
        (** Report a problem on some child node.

            If [~span] is omitted, then we will use the location of the [~source] node instead. If
            given, the fixer will be applied to the source node, rather than the current one. This
            may be useful if you are working with nodes which may appear in multiple locations (such
            as function calls). *)
  }

(** A vertex in a path from the top of a program to the leaves. *)
type path_item =
  | Expr of Syntax.expr
  | Stmt of Syntax.stmt
  | Name of Syntax.name
  | FunctionName of Syntax.function_name
  | Bind
      (** The left hand side of a variable assignment or definition. Guaranteed to be followed by a
          {!Stmt}. *)
  | Block of Syntax.block

(** Information about the current node being visited. *)
type context =
  { path : path_item list;  (** The path taken to reach this node. *)
    data : IlluaminateData.context;  (** A store for the current program data. *)
    file : File_id.t
  }

(** The primary visitor for each node a linter can consider. *)
type ('op, 'term) visitor = 'op -> context -> 'term reporter -> 'term -> unit

(** A linter is effectively a visitor which accepts some node and returns various messages for that
    specific node.

    Every linter is parameterised by an "options type" (['op]), which can be used to configure how
    the linter behaves. *)
type 'op linter_info =
  { options : 'op IlluaminateConfig.Category.key;  (** A term which parses this group's options. *)
    tags : Error.Tag.t list;  (** The tags this linter may report errors under. *)
    program : ('op, Syntax.program) visitor;
    token : ('op, Syntax.token) visitor;
    expr : ('op, Syntax.expr) visitor;
    stmt : ('op, Syntax.stmt) visitor;
    name : ('op, Syntax.name) visitor;
    var : ('op, Syntax.var) visitor;
    file : ('op, File.t) visitor
  }

(** A wrapper of {!linter_data} which hides the options type variable. *)
type t = private Linter : 'op linter_info -> t

(** Construct a new linter. *)
val make :
  options:'op IlluaminateConfig.Category.key ->
  tags:Error.Tag.t list ->
  ?program:('op, Syntax.program) visitor ->
  ?token:('op, Syntax.token) visitor ->
  ?expr:('op, Syntax.expr) visitor ->
  ?stmt:('op, Syntax.stmt) visitor ->
  ?name:('op, Syntax.name) visitor ->
  ?var:('op, Syntax.var) visitor ->
  ?file:('op, File.t) visitor ->
  unit ->
  t

(** Construct a new linter with no options *)
val make_no_opt :
  tags:Error.Tag.t list ->
  ?program:(unit, Syntax.program) visitor ->
  ?token:(unit, Syntax.token) visitor ->
  ?expr:(unit, Syntax.expr) visitor ->
  ?stmt:(unit, Syntax.stmt) visitor ->
  ?name:(unit, Syntax.name) visitor ->
  ?var:(unit, Syntax.var) visitor ->
  ?file:(unit, File.t) visitor ->
  unit ->
  t

(** The primary config category for linter options.

    Linters may chose to read options from different groups if needed. *)
val category : IlluaminateConfig.Category.t

(** A module template which can be included in a [.mli] file. *)
module type S = sig
  (** The linter exposed by this module. *)
  val linter : t
end
