open IlluaminateCore
open Linter
module C = IlluaminateConfig

type 'a witness =
  | AtExpr : Syntax.expr witness
  | AtStmt : Syntax.stmt witness
  | AtProgram : Syntax.program witness
  | AtToken : Syntax.token witness
  | AtName : Syntax.name witness
  | AtVar : Syntax.var witness

type 'a note_at =
  { note : 'a note;
    source : 'a;
    witness : 'a witness
  }

let report_note_at (type a) err ({ note; source; witness } : a note_at) =
  let span =
    match witness with
    | AtExpr -> Syntax.Spanned.expr source
    | AtStmt -> Syntax.Spanned.stmt source
    | AtToken -> Node.span source
    | AtName -> Syntax.Spanned.name source
    | AtVar ->
        let (Var n) = source in
        Node.span n
    | AtProgram -> Syntax.Spanned.program source
  in
  Error.report err note.tag (Option.value note.span ~default:span) note.message

type any_note = Note : 'a note_at -> any_note

let report_note err (Note n) = report_note_at err n

let always : Error.Tag.filter = fun _ -> true

let worker ~store ~data ~tags (Linter linter : t) program =
  let open! Syntax in
  let options = C.Schema.get linter.options store in
  let messages = ref [] in
  let visit f witness ctx source =
    match f options ctx source with
    | [] -> ()
    | xs ->
        messages :=
          List.fold_left
            (fun xs note -> if tags note.tag then Note { note; source; witness } :: xs else xs)
            !messages xs
  in
  let ( |: ) t ctx = { ctx with path = t :: ctx.path } in
  let token = visit linter.token AtToken in
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
  and stmt context stmt =
    visit linter.stmt AtStmt context stmt;
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
  and block context x = List.iter (stmt (Block x |: context)) x
  and var = visit linter.var AtVar
  and name context name =
    visit linter.name AtName context name;
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
    visit linter.expr AtExpr context expr;
    let context = Expr expr |: context in
    match expr with
    | Ref a -> name context a
    | ECall a -> call context a
    | Dots a -> token context a
    | Nil a -> token context a
    | True a -> token context a
    | False a -> token context a
    | Number _ | Int _ | String _ | MalformedNumber _ -> ()
    | Fun a -> fun_expr context a
    | Table a -> table context a
    | UnOp a -> unop_expr context a
    | BinOp a -> binop_expr context a
    | Parens a -> paren_expr context a
  and call context = function
    | Call { fn; args } -> expr context fn; call_args context args
    | Invoke { fn; colon; meth = _; args } ->
        expr context fn; token context colon; call_args context args
  and call_args context = function
    | CallArgs { open_a; args; close_a } ->
        token context open_a; list0 expr context args; token context close_a
    | CallTable a ->
        visit linter.expr AtExpr context (Table a);
        table context a
    | CallString a -> visit linter.expr AtExpr context (String a)
  and args context { args_open; args_args; args_close } =
    token context args_open;
    list0 arg (Bind |: context) args_args;
    token context args_close
  and arg context = function
    | NamedArg a -> var context a
    | DotArg a -> token context a
  in
  let context = { program; data; path = [] } in
  visit linter.program AtProgram context program;
  block context program.Syntax.program;
  token context program.eof;
  !messages

let lint ~store ~data ?(tags = always) (Linter l as linter : t) program =
  if List.exists tags l.tags then worker ~store ~data ~tags linter program else []

let fix_some ~original node note =
  if note.source != original then node
  else
    match note.note.fix with
    | FixOne f -> Result.value ~default:node (f node)
    | _ -> node

let fix prog fixes =
  let no_fix (Note { note = { fix; _ }; _ }) =
    match fix with
    | FixNothing -> true
    | FixOne _ | FixBlock _ -> false
  in
  if List.for_all no_fix fixes then prog
  else
    let open Syntax in
    (object (self)
       inherit Syntax.map as super

       method! token (tok : token) =
         let visit (type a) (x : token) (note : a note_at) : token =
           match note with
           | { witness = AtToken; _ } -> fix_some ~original:tok x note
           | _ -> x
         in
         List.fold_left (fun x (Note note) -> visit x note) tok fixes

       method! stmt stmt =
         let visit (type a) (x : stmt) (note : a note_at) : stmt =
           match note with
           | { witness = AtStmt; _ } -> fix_some ~original:stmt x note
           | _ -> x
         in
         List.fold_left (fun x (Note note) -> visit x note) stmt fixes |> super#stmt

       method! block stmts =
         let visit (type a) (original : stmt) (x : stmt) (note : a note_at) : stmt list option =
           match note with
           | { witness = AtStmt; source; note = { fix = FixBlock f; _ }; _ } when source == original
             ->
               Some (Result.value ~default:[ x ] (f x))
           | _ -> None
         in
         let rec visit_all original x = function
           | [] -> [ x ]
           | Note note :: xs -> (
             match visit original x note with
             | None -> visit_all original x xs
             | Some [ x ] -> visit_all original x xs
             | Some xs -> xs )
         in
         List.fold_right
           (fun stmt rest ->
             let x = self#stmt stmt in
             visit_all stmt x fixes @ rest)
           stmts []

       method! program program =
         let visit (type a) (x : program) (note : a note_at) : program =
           match note with
           | { witness = AtProgram; _ } -> fix_some ~original:program x note
           | _ -> x
         in
         List.fold_left (fun x (Note note) -> visit x note) program fixes |> super#program

       method! expr expr =
         let visit (type a) (x : expr) (note : a note_at) : expr =
           match note with
           | { witness = AtExpr; _ } -> fix_some ~original:expr x note
           | _ -> x
         in
         List.fold_left (fun x (Note note) -> visit x note) expr fixes |> super#expr

       method! call_args args =
         let rewrap = function
           | Table t -> CallTable t
           | String t -> CallString t
           | e ->
               CallArgs
                 { open_a = SimpleNode { contents = OParen };
                   args = Some (Mono e);
                   close_a = SimpleNode { contents = CParen }
                 }
         in
         let fix { fix; _ } args expr =
           match fix with
           | FixOne f -> f expr |> Result.fold ~error:(fun _ -> args) ~ok:rewrap
           | _ -> args
         in

         let args =
           match args with
           | CallArgs _ -> args
           | CallTable original ->
               let rec visit notes (arg : call_args) : call_args =
                 match (arg, notes) with
                 | _, [] -> arg
                 | (CallArgs _ | CallString _), _ -> arg (* If we're no longer a table, abort. *)
                 | CallTable t, Note { witness = AtExpr; source; note } :: notes ->
                     ( match source with
                     | Table source when source == original -> fix note args (Table t)
                     | _ -> args )
                     |> visit notes
                 | CallTable _, _ :: notes -> visit notes arg
               in
               visit fixes args
           | CallString original ->
               let rec visit notes (arg : call_args) : call_args =
                 match (arg, notes) with
                 | _, [] -> arg
                 | (CallArgs _ | CallTable _), _ -> arg (* If we're no longer a string, abort. *)
                 | CallString s, Note { witness = AtExpr; source; note } :: notes ->
                     ( match source with
                     | String source when source == original -> fix note args (String s)
                     | _ -> args )
                     |> visit notes
                 | CallString _, _ :: notes -> visit notes arg
               in
               visit fixes args
         in
         super#call_args args

       method! name name =
         let visit (type a) (x : name) (note : a note_at) : name =
           match note with
           | { witness = AtName; _ } -> fix_some ~original:name x note
           | _ -> x
         in
         List.fold_left (fun x (Note note) -> visit x note) name fixes |> super#name

       method! var var =
         let visit (type a) (x : var) (note : a note_at) : var =
           match note with
           | { witness = AtVar; _ } -> fix_some ~original:var x note
           | _ -> x
         in
         List.fold_left (fun x (Note note) -> visit x note) var fixes |> super#var
    end)
      #program prog

let lint_and_fix_all ~store ~files ?id ?tags linters program =
  let program, notes, _ =
    List.fold_left
      (fun (program, msgs, files) linter ->
        let msgs' = lint ~store ~data:(Data.of_files files) ?tags linter program in
        let program = fix program msgs' in
        let files =
          match id with
          | None -> files
          | Some id -> Data.Files.update id program files
        in
        (program, msgs' @ msgs, files))
      (program, [], files) linters
  in

  (program, notes)
