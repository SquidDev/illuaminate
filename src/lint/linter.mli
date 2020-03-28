(** The base type for linters, and the information they produce. *)

open IlluaminateCore

(** A fixer for a problematic node. This should map a term of some type, to an equivalent
    no-problematic term. *)
type 'a fixer =
  | FixNothing : 'a fixer  (** This node has no fixer. *)
  | FixOne : ('a -> ('a, string) result) -> 'a fixer
      (** This node can be fixed by replacing it with a single equivalent node. If the node cannot
          be fixed, we return {!Error}. *)
  | FixBlock : (Syntax.stmt -> (Syntax.stmt list, string) result) -> Syntax.stmt fixer
      (** This statement can be fixed by replacing it with 0 or more statements. *)

(** A linter message which will be attached to a specific node. *)
type 'a note = private
  { message : string;  (** The message used within this warning. *)
    detail : (Format.formatter -> unit) option;  (** Additional detail to the message. *)
    fix : 'a fixer;
        (** A function which will be used to fix up this warning. Note, this function should not
            make any assumptions about the form of this node. *)
    tag : Error.Tag.t;  (** The tag associated with this error. *)
    span : Span.t option  (** An optional span to override the node. *)
  }

(** Construct a new {!type:note} *)
val note :
  ?fix:'a fixer ->
  ?span:Span.t ->
  ?detail:(Format.formatter -> unit) ->
  tag:Error.Tag.t ->
  ('f, Format.formatter, unit, 'a note) format4 ->
  'f

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
    program : Syntax.program
  }

(** The primary visitor for each node a linter can consider. *)
type ('op, 'term) visitor = 'op -> context -> 'term -> 'term note list

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
    var : ('op, Syntax.var) visitor
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
