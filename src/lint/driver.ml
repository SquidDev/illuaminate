open IlluaminateCore
open Linter
module C = IlluaminateConfig

module Note = struct
  type 'a t =
    { error : Illuaminate.Error.t;  (** The actual error. *)
      source : 'a;
      fix : 'a Fixer.t;
      kind : 'a Witness.t
    }

  let to_error { error; _ } = error

  type any = Note : 'a t -> any [@@unboxed]

  let any_to_error (Note n) = to_error n
end

module Node = struct
  type t = Node : 'a Witness.t * 'a -> t

  let hash = Hashtbl.hash

  let equal (Node (lw, l)) (Node (rw, r)) =
    match (lw, rw) with
    | Args, Args -> l == r
    | BinOp, BinOp -> l == r
    | Call, Call -> l == r
    | CallArgs, CallArgs -> l == r
    | Name, Name -> l == r
    | FunctionName, FunctionName -> l == r
    | Program, Program -> l == r
    | TableItem, TableItem -> l == r
    | Token, Token -> l == r
    | Var, Var -> l == r
    | Stmt, Stmt -> l == r
    | Expr, Expr -> (
      match (l, r) with
      | String l, String r -> l == r
      | Table l, Table r -> l == r
      | l, r -> l == r)
    | File, File -> File.equal l r
    | ( ( Stmt
        | Program
        | Token
        | BinOp
        | Name
        | FunctionName
        | Expr
        | Var
        | Call
        | Args
        | CallArgs
        | TableItem
        | File ),
        _ ) -> false
  [@@coverage off]
end

let extract (type a) (l : a Witness.t) (Note.Note ({ kind = r; _ } as note)) : a Note.t =
  match (l, r) with
  | Args, Args -> note
  | BinOp, BinOp -> note
  | Call, Call -> note
  | CallArgs, CallArgs -> note
  | Expr, Expr -> note
  | FunctionName, FunctionName -> note
  | Name, Name -> note
  | Program, Program -> note
  | Stmt, Stmt -> note
  | TableItem, TableItem -> note
  | Token, Token -> note
  | Var, Var -> note
  | File, File -> note
  | ( ( Stmt
      | Program
      | Token
      | BinOp
      | Name
      | Expr
      | FunctionName
      | Var
      | Call
      | Args
      | CallArgs
      | TableItem
      | File ),
      _ ) -> failwith "Witness mismatch!"
[@@coverage off]

module Notes = struct
  module Tbl = Hashtbl.Make (Node)

  type t = Note.any Tbl.t

  let empty : t = Tbl.create 0
  let size = Tbl.length
  let find witness x t = Tbl.find_all t (Node.Node (witness, x)) |> List.rev_map (extract witness)
  let to_seq = Tbl.to_seq_values
end

let always : Error.Tag.filter = fun _ -> true

type r =
  { x : Illuaminate.Error.t -> unit;
    r :
      'a 'b.
      ?fix:'a Linter.Fixer.t ->
      ?span:IlluaminateCore.Span.t ->
      ?detail:(Format.formatter -> unit) ->
      tag:IlluaminateCore.Error.Tag.t ->
      kind:'a IlluaminateCore.Witness.t ->
      source:'a ->
      ('b, Format.formatter, unit, unit, unit, unit) CamlinternalFormatBasics.format6 ->
      'b
  }

let program_worker ~options ~context ~r:({ r; x } : r) linter program =
  let open! Syntax in
  let visit (type a) (f : (_, a) Linter.visitor) (kind : a Witness.t) ctx (source : a) =
    f options ctx { r = r ~kind ~source; e = r; x } source
  in
  let ( |: ) t ctx = { ctx with path = t :: ctx.path } in
  let token = visit linter.token Token in
  let list1 f ctx = SepList1.iter (f ctx) ~tok:(token ctx) in
  let list0 f ctx = Option.iter (list1 f ctx) in
  (* Fear not, this code is mostly auto-generated. *)
  let rec do_stmt context { do_do; do_body; do_end } =
    token context do_do; block context do_body; token context do_end
  and assign_stmt context { assign_vars; assign_eq; assign_vals } =
    list1 name (Bind |: context) assign_vars;
    token context assign_eq;
    list1 expr context assign_vals
  and while_stmt context { while_while; while_test; while_do; while_body; while_end } =
    token context while_while;
    expr context while_test;
    token context while_do;
    block context while_body;
    token context while_end
  and repeat_stmt context { repeat_repeat; repeat_body; repeat_until; repeat_test } =
    token context repeat_repeat;
    block context repeat_body;
    token context repeat_until;
    expr context repeat_test
  and for_num_stmt context
      { forn_for;
        forn_var;
        forn_eq;
        forn_start;
        forn_comma;
        forn_limit;
        forn_step;
        forn_do;
        forn_body;
        forn_end
      } =
    token context forn_for;
    var (Bind |: context) forn_var;
    token context forn_eq;
    expr context forn_start;
    token context forn_comma;
    expr context forn_limit;
    Option.iter (fun (t, e) -> token context t; expr context e) forn_step;
    token context forn_do;
    block context forn_body;
    token context forn_end
  and for_in_stmt context { forp_for; forp_vars; forp_in; forp_iter; forp_do; forp_body; forp_end }
      =
    token context forp_for;
    list1 var (Bind |: context) forp_vars;
    token context forp_in;
    list1 expr context forp_iter;
    token context forp_do;
    block context forp_body;
    token context forp_end
  and local_stmt context { local_local; local_vars; local_vals } =
    token context local_local;
    list1 var (Bind |: context) local_vars;
    Option.iter (fun (t, e) -> token context t; list1 expr context e) local_vals
  and local_function_stmt context
      { localf_local; localf_function; localf_var; localf_args; localf_body; localf_end } =
    token context localf_local;
    token context localf_function;
    var (Bind |: context) localf_var;
    args context localf_args;
    block context localf_body;
    token context localf_end
  and function_stmt context
      { assignf_function; assignf_name; assignf_args; assignf_body; assignf_end } =
    token context assignf_function;
    function_name (Bind |: context) assignf_name;
    args context assignf_args;
    block context assignf_body;
    token context assignf_end
  and function_name context node =
    let context = FunctionName node |: context in
    match node with
    | FVar a -> var context a
    | FDot { tbl; dot; field = _ } -> function_name context tbl; token context dot
    | FSelf { tbl; colon; meth = _ } -> function_name context tbl; token context colon
  and return_stmt context { return_return; return_vals } =
    token context return_return; list0 expr context return_vals
  and if_stmt context { if_if; if_elseif; if_else; if_end } =
    if_clause context if_if;
    List.iter (if_clause context) if_elseif;
    Option.iter (fun (t, b) -> token context t; block context b) if_else;
    token context if_end
  and if_clause context { clause_if; clause_test; clause_then; clause_body } =
    token context clause_if;
    expr context clause_test;
    token context clause_then;
    block context clause_body
  and goto context { goto_goto; goto_label = _ } = token context goto_goto
  and label context { label_start; label_name = _; label_finish } =
    token context label_start; token context label_finish
  and stmt context stmt =
    visit linter.stmt Stmt context stmt;
    let context = Stmt stmt |: context in
    match stmt with
    | Do a -> do_stmt context a
    | Assign a -> assign_stmt context a
    | While a -> while_stmt context a
    | Repeat a -> repeat_stmt context a
    | ForNum a -> for_num_stmt context a
    | ForIn a -> for_in_stmt context a
    | Local a -> local_stmt context a
    | LocalFunction a -> local_function_stmt context a
    | AssignFunction a -> function_stmt context a
    | Return a -> return_stmt context a
    | If a -> if_stmt context a
    | Break a -> token context a
    | SCall a -> call context a
    | Semicolon a -> token context a
    | Goto a -> goto context a
    | Label a -> label context a
  and block context x = List.iter (stmt (Block x |: context)) x
  and var = visit linter.var Var
  and name context name =
    visit linter.name Name context name;
    let context = Name name |: context in
    match name with
    | NVar a -> var context a
    | NDot { tbl; dot; key = _ } -> expr context tbl; token context dot
    | NLookup { tbl; open_k; key; close_k } ->
        expr context tbl; token context open_k; expr context key; token context close_k
  and fun_expr context { fun_function; fun_args; fun_body; fun_end } =
    token context fun_function; args context fun_args; block context fun_body; token context fun_end
  and table context { table_open; table_body; table_close } =
    token context table_open;
    List.iter
      (fun (x, t) ->
        table_item context x;
        Option.iter (token context) t)
      table_body;
    token context table_close
  and table_item context = function
    | Array a -> expr context a
    | RawPair { ident = _; eq; value } -> token context eq; expr context value
    | ExprPair { open_k; key; close_k; eq; value } ->
        token context open_k;
        expr context key;
        token context close_k;
        token context eq;
        expr context value
  and unop_expr context { unop_op = _; unop_rhs } = expr context unop_rhs
  and binop_expr context { binop_lhs; binop_op = _; binop_rhs } =
    expr context binop_lhs; expr context binop_rhs
  and paren_expr context { paren_open; paren_expr; paren_close } =
    token context paren_open; expr context paren_expr; token context paren_close
  and expr context expr =
    visit linter.expr Expr context expr;
    let context = Expr expr |: context in
    match expr with
    | Ref a -> name context a
    | ECall a -> call context a
    | Dots a -> token context a
    | Nil a -> token context a
    | True a -> token context a
    | False a -> token context a
    | Number _ | String _ -> ()
    | Fun a -> fun_expr context a
    | Table a -> table context a
    | UnOp a -> unop_expr context a
    | BinOp a -> binop_expr context a
    | Parens a -> paren_expr context a
  and call context = function
    | Call { fn; args } -> expr context fn; call_args context args
    | Invoke { obj; colon; meth = _; args } ->
        expr context obj; token context colon; call_args context args
  and call_args context = function
    | CallArgs { open_a; args; close_a } ->
        token context open_a; list0 expr context args; token context close_a
    | CallTable a ->
        visit linter.expr Expr context (Table a);
        table context a
    | CallString a -> visit linter.expr Expr context (String a)
  and args context { args_open; args_args; args_close } =
    token context args_open;
    list0 arg (Bind |: context) args_args;
    token context args_close
  and arg context = function
    | NamedArg a -> var context a
    | DotArg a -> token context a
  in

  visit linter.program Program context program;
  block context program.Syntax.program;
  token context program.eof

let worker ~store ~data ~tags (Linter linter : t) document =
  let options = C.Schema.get linter.options store in
  let messages = Notes.Tbl.create 16 in
  let r ?(fix = Fixer.none) ?span ?detail ~(tag : IlluaminateCore.Error.Tag.t) ~kind ~source message
      =
    if tags tag then
      let span =
        match span with
        | Some span -> span
        | None -> Witness.span kind source
      in
      Format.kasprintf
        (fun message ->
          let error =
            Illuaminate.Error.v ~code:tag.name ~severity:tag.level
              ~tags:(List.filter_map IlluaminateCore.Error.tag_of_attribute tag.attributes)
              (Span.to_error_position span) (Fmt.const Fmt.string message) []
              (Option.map (fun f out () -> f out) detail)
          in
          Notes.Tbl.add messages (Node (kind, source)) (Note.Note { error; fix; source; kind }))
        message
    else Format.ifprintf Format.std_formatter message
  in
  let x error =
    Notes.Tbl.add messages
      (Node (File, document))
      (Note.Note { error; kind = File; source = document; fix = Fixer.none })
  in

  let context = { data; path = []; file = File.span document |> Span.filename } in
  (match document with
  | File.Lua l -> program_worker ~options ~context ~r:{ r; x } linter l
  | _ -> ());
  linter.file options context { r = r ~kind:File ~source:document; e = r; x } document;
  messages

let need_lint ~store ~data ?(tags = always) (Linter l as linter : t) program =
  if l.tags = [] || List.exists tags l.tags then worker ~store ~data ~tags linter program
  else Notes.empty

let lint ~store ~data ?tags l program =
  IlluaminateData.compute (fun data -> need_lint ~store ~data ?tags l program) data

let fix_all (type a) fixes (w : a Witness.t) (original : a) : a =
  let rec go node = function
    | [] -> node
    | x :: xs -> (
      match extract w x with
      | { fix = Nothing | Block _; _ } -> go node xs
      | { fix = One f; _ } ->
          let node' = Result.value ~default:node (f node) in
          if node' != node then node' else go node xs)
  in
  Notes.Tbl.find_all fixes (Node (w, original)) |> go original

let rec no_fixers (f : Note.any Seq.t) : bool =
  match f () with
  | Nil -> true
  | Cons (Note { fix = One _ | Block _; _ }, _) -> false
  | Cons (Note { fix = Nothing; _ }, f) -> no_fixers f

let fix_program prog (fixes : Notes.t) =
  if Notes.to_seq fixes |> no_fixers then prog
  else
    let open Syntax in
    let obj =
      object
        inherit Syntax.map as super
        method! token (tok : token) = fix_all fixes Token tok
        method! stmt x = fix_all fixes Stmt x |> super#stmt

        method! block =
          let rec go rest node = function
            | [] -> super#stmt node :: rest
            | x :: xs -> (
              match extract Stmt x with
              | { fix = Nothing; _ } -> go rest node xs
              | { fix = Block f; _ } -> (
                  let node' = f node in
                  match node' with
                  | Error _ -> go rest node xs
                  | Ok stmts ->
                      (* There's a risk here that this will result in an infinite loop, if the fixer
                         returns a list of statements which contains the original one. So don't do
                         that, OK? *)
                      go_block rest stmts)
              | { fix = One f; _ } ->
                  let node' = Result.value ~default:node (f node) in
                  if node' != node then super#stmt node' :: rest else go rest node xs)
          and go_stmt original xs =
            Notes.Tbl.find_all fixes (Node (Stmt, original)) |> go xs original
          and go_block rest xs = List.fold_right go_stmt xs rest in
          go_block []

        method! program program = fix_all fixes Program program |> super#program
        method! call x = fix_all fixes Call x |> super#call
        method! expr x = fix_all fixes Expr x |> super#expr
        method! args x = fix_all fixes Args x |> super#args
        method! table_item x = fix_all fixes TableItem x |> super#table_item

        method! call_args args =
          let args = fix_all fixes CallArgs args in
          let rewrap = function
            | Table t -> CallTable t
            | String t -> CallString t
            | e ->
                let empty = Illuaminate.IArray.empty in
                CallArgs
                  { open_a =
                      { contents = OParen;
                        leading_trivia = empty;
                        trailing_trivia = empty;
                        span = First.expr.get e |> IlluaminateCore.Node.span |> Span.start
                      };
                    args = Some (Mono e);
                    close_a =
                      { contents = CParen;
                        leading_trivia = empty;
                        trailing_trivia = empty;
                        span = First.expr.get e |> IlluaminateCore.Node.span |> Span.finish
                      }
                  }
          in
          let args =
            match args with
            | CallArgs _ -> args
            | CallTable original -> fix_all fixes Expr (Table original) |> rewrap
            | CallString original -> fix_all fixes Expr (String original) |> rewrap
          in
          super#call_args args

        method! name name = fix_all fixes Name name |> super#name
        method! var var = fix_all fixes Var var |> super#var
      end
    in
    obj#program prog

let fix file (fixes : Notes.t) =
  if Notes.to_seq fixes |> no_fixers then file
  else
    let file = fix_all fixes File file in
    match file with
    | File.Lua x -> Lua (fix_program x fixes)
    | x -> x

let lint_and_fix_all ~store ~data ?files ?tags linters program =
  let all = Notes.Tbl.create 8 in
  let program =
    List.fold_left
      (fun program linter ->
        let msgs' = lint ~store ~data ?tags linter program in
        let program = fix program msgs' in
        Option.iter
          (fun (name, store) ->
            IlluaminateData.Programs.FileStore.update store name (Some program);
            IlluaminateData.refresh data)
          files;
        Notes.Tbl.(iter (add all) msgs');
        program)
      program linters
  in
  (program, all)
