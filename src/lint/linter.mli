(** The base type for linters, and the information they produce. *)

open IlluaminateCore

type 'a fixer =
  | FixNothing : 'a fixer
  | FixOne : ('a -> ('a, string) result) -> 'a fixer
  | FixBlock : (Syntax.stmt -> (Syntax.stmt list, string) result) -> Syntax.stmt fixer

(** A linter message which will be attached to a specific node. *)
type 'a note = private
  { message : string;  (** The message used within this warning. *)
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
[@@deriving show]

(** Information about the current node being visited. *)
type context =
  { path : path_item list;  (** The path taken to reach this node. *)
    data : IlluaminateSemantics.Data.t;  (** A store for the current program data. *)
    program : Syntax.program
  }

(** The primary visitor for each node a linter can consider. *)
type ('op, 'term) visitor = 'op -> context -> 'term -> 'term note list

(** A linter is effectively a visitor which accepts some node and returns various messages for that
    specific node.

    Every linter is parameterised by an option type (['op]), which can be used to configure how the
    linter behaves. *)
type 'op linter_info =
  { options : 'op IlluaminateConfig.Category.key;  (** A term which parses this group's options. *)
    tags : Error.Tag.t list;  (** The tags this linter may report errors under.*)
    program : ('op, Syntax.program) visitor;
    token : ('op, Syntax.token) visitor;
    expr : ('op, Syntax.expr) visitor;
    stmt : ('op, Syntax.stmt) visitor;
    name : ('op, Syntax.name) visitor;
    var : ('op, Syntax.var) visitor
  }

(** A wrapper of {!linter_data} which hides the option type variable. *)
type t = private Linter : 'a linter_info -> t

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
