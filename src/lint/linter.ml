open IlluaminateCore

module Fixer = struct
  type 'a t =
    | Nothing : 'a t
    | One : ('a -> ('a, string) result) -> 'a t
    | Block : (Syntax.stmt -> (Syntax.stmt list, string) result) -> Syntax.stmt t

  let none = Nothing
  let fix f = One f
  let block f = Block f
end

type 'a reporter =
  { r :
      'f.
      ?fix:'a Fixer.t ->
      ?span:Span.t ->
      ?detail:(Format.formatter -> unit) ->
      tag:Error.Tag.t ->
      ('f, Format.formatter, unit, unit) format4 ->
      'f;
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
  }

type path_item =
  | Expr of Syntax.expr
  | Stmt of Syntax.stmt
  | Name of Syntax.name
  | FunctionName of Syntax.function_name
  | Bind
  | Block of Syntax.block

type context =
  { path : path_item list;
    data : IlluaminateData.context;
    file : Span.filename
  }

type ('op, 'term) visitor = 'op -> context -> 'term reporter -> 'term -> unit

let default_visitor : ('op, 'term) visitor = fun _ _ _ _ -> ()

type 'op linter_info =
  { options : 'op IlluaminateConfig.Category.key;
    tags : IlluaminateCore.Error.Tag.t list;
    program : ('op, Syntax.program) visitor;
    token : ('op, Syntax.token) visitor;
    expr : ('op, Syntax.expr) visitor;
    stmt : ('op, Syntax.stmt) visitor;
    name : ('op, Syntax.name) visitor;
    var : ('op, Syntax.var) visitor;
    file : ('op, File.t) visitor
  }

type t = Linter : 'a linter_info -> t

let make ~options ~tags ?(program = default_visitor) ?(token = default_visitor)
    ?(expr = default_visitor) ?(stmt = default_visitor) ?(name = default_visitor)
    ?(var = default_visitor) ?(file = default_visitor) () =
  Linter { options; tags; program; token; expr; stmt; name; var; file }

let category =
  IlluaminateConfig.Category.create ~name:"lint"
    ~comment:"Control how the illuaminate linter works." ()

let make_no_opt = make ~options:IlluaminateConfig.(Category.add Term.unit category)

module type S = sig
  val linter : t
end
