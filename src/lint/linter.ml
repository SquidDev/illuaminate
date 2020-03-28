open IlluaminateCore

type 'a fixer =
  | FixNothing : 'a fixer
  | FixOne : ('a -> ('a, string) result) -> 'a fixer
  | FixBlock : (Syntax.stmt -> (Syntax.stmt list, string) result) -> Syntax.stmt fixer

type 'a note =
  { message : string;
    detail : (Format.formatter -> unit) option;
    fix : 'a fixer;
    tag : Error.Tag.t;
    span : Span.t option
  }

let note ?(fix = FixNothing) ?span ?detail ~tag message =
  Format.kasprintf (fun message -> { fix; message; detail; span; tag }) message

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
    program : Syntax.program
  }

type ('op, 'term) visitor = 'op -> context -> 'term -> 'term note list

let default_visitor : ('op, 'term) visitor = fun _ _ _ -> []

type 'op linter_info =
  { options : 'op IlluaminateConfig.Category.key;
    tags : IlluaminateCore.Error.Tag.t list;
    program : ('op, Syntax.program) visitor;
    token : ('op, Syntax.token) visitor;
    expr : ('op, Syntax.expr) visitor;
    stmt : ('op, Syntax.stmt) visitor;
    name : ('op, Syntax.name) visitor;
    var : ('op, Syntax.var) visitor
  }

type t = Linter : 'a linter_info -> t

let make ~options ~tags ?(program = default_visitor) ?(token = default_visitor)
    ?(expr = default_visitor) ?(stmt = default_visitor) ?(name = default_visitor)
    ?(var = default_visitor) () =
  Linter { options; tags; program; token; expr; stmt; name; var }

let category =
  IlluaminateConfig.Category.create ~name:"lint"
    ~comment:"Control how the illuaminate linter works." ()

let make_no_opt = make ~options:IlluaminateConfig.(Category.add Term.unit category)

module type S = sig
  val linter : t
end
