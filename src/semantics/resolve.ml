module StringMap = Map.Make (String)
module S = IlluaminateCore.Syntax
open IlluaminateCore
open Lens

type scope =
  { scope_id : int;
    fun_id : int
  }

type function_scope = scope

type kind =
  | Global
  | Arg of
      { scope : function_scope;
        def : Syntax.var
      }
  | ImplicitArg of
      { scope : function_scope;
        kind : [ `Self | `Arg ];
        def : Syntax.args
      }
  | Local of
      { scope : scope;
        def : Syntax.var
      }
  | Loop of
      { scope : scope;
        def : Syntax.var
      }

module Kind = struct
  type t = kind

  let definition = function
    | ImplicitArg { def; _ } -> Some (Syntax.Spanned.args def)
    | Loop { def; _ } | Local { def; _ } | Arg { def; _ } -> Some (Syntax.Spanned.var def)
    | Global -> None
end

type definition =
  | Declare
  | OfExpr of Syntax.expr
  | OfSelect of int * Syntax.expr
  | OfFunction of Syntax.args * Syntax.block

type var =
  { name : string;
    kind : kind;
    shadows : var option;
    mutable usages : var_usage list;
    mutable definitions : (var_usage option * definition) list;
    mutable captured : bool;
    mutable upvalue_mutated : bool
  }

and var_usage =
  { var : var;
    node : Syntax.var;
    snapshot : var StringMap.t
  }

type dots =
  { dot_scope : function_scope;
    dot_node : Syntax.token option;
    dot_implicit : var option;
    mutable dot_usages : dots_usage list
  }

and dots_usage =
  | IllegalDots
  | BoundDots of
      { dots : dots;
        node : Syntax.token
      }

module SVarTbl = Hashtbl.Make (struct
  type t = S.var

  let equal = ( == )
  let hash (S.Var n) = Node.span n ^. Span.start_offset
end)

module TokenTbl = Hashtbl.Make (struct
  type t = S.token

  let equal = ( == )
  let hash t = CCHash.poly (Node.span t)
end)

type t =
  { var_defs : var SVarTbl.t;
    var_usages : var_usage SVarTbl.t;
    dots_defs : dots TokenTbl.t;
    dots_usages : dots_usage TokenTbl.t;
    globals : (string, var) Hashtbl.t;
    latest_scope_id : int ref;
    latest_fun_id : int ref
  }

type context =
  { variables : var StringMap.t;
    store : t;
    active_dots : dots option;
    active_scope : scope
  }

let mk_local' kind context name =
  let var =
    { name;
      kind;
      shadows = StringMap.find_opt name context.variables;
      usages = [];
      definitions = [];
      captured = false;
      upvalue_mutated = false
    }
  in
  ({ context with variables = StringMap.add name var context.variables }, var)

let mk_local kind context (S.Var name as syntax) =
  let name = name ^. Node.contents in
  let context, var = mk_local' kind context name in
  (context, (var, { var; node = syntax; snapshot = context.variables }))

let add_def context (var, use) def =
  var.definitions <- (Some use, def) :: var.definitions;
  SVarTbl.add context.store.var_defs use.node var;
  match var.kind with
  | Global -> ()
  | Local { scope; _ } | Loop { scope; _ } | Arg { scope; _ } | ImplicitArg { scope; _ } ->
      if scope.fun_id <> context.active_scope.fun_id then var.upvalue_mutated <- true

let mk_locals kind = S.SepList1.map_with' (fun s v -> mk_local (kind v) s v)

let mk_arg args context (arg : S.arg) =
  match arg with
  | S.NamedArg name ->
      let context, var = mk_local (Arg { scope = context.active_scope; def = name }) context name in
      add_def context var Declare; context
  | S.DotArg dot ->
      let context, arg =
        mk_local'
          (ImplicitArg { scope = context.active_scope; def = args; kind = `Arg })
          context "arg"
      in
      arg.definitions <- (None, Declare) :: arg.definitions;
      let var =
        { dot_scope = context.active_scope;
          dot_node = Some dot;
          dot_usages = [];
          dot_implicit = Some arg
        }
      in
      TokenTbl.add context.store.dots_defs dot var;
      { context with active_dots = Some var }

let mk_args s ({ S.args_args; _ } as a) = S.SepList0.fold_left (mk_arg a) s args_args

let get_and_incr x =
  let v = !x in
  x := v + 1;
  v

let fresh_scope context =
  { context with
    active_scope =
      { context.active_scope with scope_id = get_and_incr context.store.latest_scope_id }
  }

let fresh_fun context =
  let scope =
    { scope_id = get_and_incr context.store.latest_scope_id;
      fun_id = get_and_incr context.store.latest_fun_id
    }
  in
  { context with active_scope = scope; active_dots = None }

let find_var s (S.Var var) =
  let var = var ^. Node.contents in
  match StringMap.find_opt var s.variables with
  | Some v -> v
  | None -> (
    match Hashtbl.find_opt s.store.globals var with
    | Some v -> v
    | None ->
        let v =
          { name = var;
            kind = Global;
            shadows = None;
            usages = [];
            definitions = [];
            captured = false;
            upvalue_mutated = false
          }
        in
        Hashtbl.add s.store.globals var v; v)

let var_usage s node =
  let var = find_var s node in
  (var, { var; node; snapshot = s.variables })

let use_var s name =
  let var, usage = var_usage s name in
  var.usages <- usage :: var.usages;
  SVarTbl.add s.store.var_usages name usage;
  match var.kind with
  | Global -> ()
  | Local { scope; _ } | Loop { scope; _ } | Arg { scope; _ } | ImplicitArg { scope; _ } ->
      if s.active_scope.fun_id <> scope.fun_id then var.captured <- true

let rec resolve_stmts scope stmts = CCList.fold_left resolve_stmt scope stmts |> ignore
and resolve_exprs scope = S.SepList1.iter (resolve_expr scope)

and resolve_stmt s (stmt : S.stmt) =
  match stmt with
  | S.Do { S.do_body = bk; _ } ->
      resolve_stmts (fresh_scope s) bk;
      s
  | Assign { assign_vars = vs; assign_vals = es; _ } ->
      let maybe_bind v e =
        match v with
        | S.NVar v -> add_def s (var_usage s v) e
        | n -> resolve_name s n
      in
      let rec go vs es =
        let open S.SepList1 in
        match (vs, es) with
        | Mono v, (Mono e | Cons1 (e, _, _)) -> maybe_bind v (OfExpr e)
        | Cons1 (v, _, vs), Cons1 (e, _, es) -> maybe_bind v (OfExpr e); go vs es
        | Cons1 (v, _, vs), Mono e ->
            maybe_bind v (OfExpr e);
            iteri (fun i v -> maybe_bind v (OfSelect (i + 1, e))) vs
      in
      resolve_exprs s es; go vs es; s
  | While { while_test = t; while_body = xs; _ } ->
      resolve_expr s t;
      resolve_stmts (fresh_scope s) xs;
      s
  | Repeat { repeat_body = xs; repeat_test = t; _ } ->
      let scope = CCList.fold_left resolve_stmt (fresh_scope s) xs in
      resolve_expr scope t; s
  | ForNum { forn_var; forn_start; forn_limit; forn_step; forn_body; _ } ->
      let s' = fresh_scope s in
      let scope, counter =
        mk_local (Loop { scope = s'.active_scope; def = forn_var }) s' forn_var
      in
      add_def scope counter Declare;
      resolve_expr s forn_start;
      resolve_expr s forn_limit;
      Option.iter (fun (_, x) -> resolve_expr s x) forn_step;
      resolve_stmts scope forn_body;
      s
  | ForIn { forp_vars; forp_iter; forp_body; _ } ->
      let s' = fresh_scope s in
      let scope, names =
        mk_locals (fun def -> Loop { scope = s'.active_scope; def }) s' forp_vars
      in
      List.iter (fun d -> add_def scope d Declare) names;
      resolve_exprs s forp_iter;
      resolve_stmts scope forp_body;
      s
  | Local { local_vars; local_vals; _ } ->
      let var_scope, vs =
        mk_locals (fun def -> Local { scope = s.active_scope; def }) s local_vars
      in
      (match local_vals with
      | None -> List.iter (fun v -> add_def s v Declare) vs
      | Some (_, es) ->
          let rec go vs es =
            let open S.SepList1 in
            match (vs, es) with
            | [], _ -> ()
            | [ v ], (Mono e | Cons1 (e, _, _)) -> add_def s v (OfExpr e)
            | v :: vs, Cons1 (e, _, es) -> add_def s v (OfExpr e); go vs es
            | v :: vs, Mono e ->
                add_def s v (OfExpr e);
                List.iteri (fun i v -> add_def s v (OfSelect (i + 1, e))) vs
          in
          resolve_exprs s es; go vs es);
      var_scope
  | LocalFunction { localf_var; localf_args; localf_body; _ } ->
      let var_scope, name =
        mk_local (Local { scope = s.active_scope; def = localf_var }) s localf_var
      in
      add_def var_scope name (OfFunction (localf_args, localf_body));
      let fun_scope = mk_args (fresh_fun var_scope) localf_args in
      resolve_stmts fun_scope localf_body;
      var_scope
  | AssignFunction { assignf_name; assignf_args; assignf_body; _ } ->
      (match resolve_function_name s assignf_name with
      | Some v -> add_def s v (OfFunction (assignf_args, assignf_body))
      | None -> ());
      let fun_scope = mk_args (fresh_fun s) assignf_args in
      let fun_scope =
        match assignf_name with
        | FVar _ | FDot _ -> fun_scope
        | FSelf _ ->
            let context, var =
              mk_local'
                (ImplicitArg { scope = fun_scope.active_scope; kind = `Self; def = assignf_args })
                fun_scope "self"
            in
            var.definitions <- (None, Declare) :: var.definitions;
            context
      in
      resolve_stmts fun_scope assignf_body;
      s
  | If { if_if; if_elseif; if_else; _ } ->
      let clause { S.clause_test; clause_body; _ } =
        resolve_expr s clause_test;
        resolve_stmts (fresh_scope s) clause_body
      in
      clause if_if;
      List.iter clause if_elseif;
      Option.iter (fun (_, x) -> resolve_stmts s x) if_else;
      s
  | Return { return_vals; _ } ->
      S.SepList0.iter (resolve_expr s) return_vals;
      s
  | SCall call -> resolve_call s call; s
  | Break _ | Semicolon _ | Goto _ | Label _ -> s

and resolve_function_name s = function
  | S.FVar var -> Some (var_usage s var)
  | n ->
      let rec go = function
        | S.FVar var -> use_var s var
        | FDot { tbl; _ } | FSelf { tbl; _ } -> go tbl
      in
      go n; None

and resolve_expr s = function
  | S.Ref var -> resolve_name s var
  | ECall call -> resolve_call s call
  | Nil _ | True _ | False _ | Number _ | Int _ | MalformedNumber _ | String _ -> ()
  | Dots node ->
      let dots =
        match s.active_dots with
        | None -> IllegalDots
        | Some dots ->
            let usage = BoundDots { dots; node } in
            dots.dot_usages <- usage :: dots.dot_usages;
            usage
      in
      TokenTbl.add s.store.dots_usages node dots
  | Fun { fun_args; fun_body; _ } ->
      let scope = mk_args (fresh_fun s) fun_args in
      resolve_stmts scope fun_body
  | Table { table_body; _ } ->
      let go = function
        | S.Array x -> resolve_expr s x
        | RawPair { value; _ } -> resolve_expr s value
        | ExprPair { key; value; _ } -> resolve_expr s key; resolve_expr s value
      in
      List.iter (fun (x, _) -> go x) table_body
  | UnOp { unop_op = _; unop_rhs } -> resolve_expr s unop_rhs
  | BinOp { binop_lhs; binop_op = _; binop_rhs } ->
      resolve_expr s binop_lhs; resolve_expr s binop_rhs
  | Parens { paren_expr = e; _ } -> resolve_expr s e

and resolve_name s syntax =
  match syntax with
  | S.NVar var -> use_var s var
  | NDot { tbl; _ } -> resolve_expr s tbl
  | NLookup { tbl; key; _ } -> resolve_expr s tbl; resolve_expr s key

and resolve_call s = function
  | S.Call { fn; args } -> resolve_expr s fn; resolve_call_args s args
  | Invoke { obj; args; _ } -> resolve_expr s obj; resolve_call_args s args

and resolve_call_args s = function
  | S.CallArgs { args; _ } -> S.SepList0.iter (resolve_expr s) args
  | CallTable x -> resolve_expr s (S.Table x)
  | CallString x -> resolve_expr s (S.String x)

let compute { S.program; _ } =
  let store =
    { var_defs = SVarTbl.create 32;
      var_usages = SVarTbl.create 32;
      dots_defs = TokenTbl.create 32;
      dots_usages = TokenTbl.create 32;
      globals = Hashtbl.create 32;
      latest_scope_id = ref 1;
      latest_fun_id = ref 1
    }
  and scope = { scope_id = 0; fun_id = 0 } in
  let context =
    { variables = StringMap.empty;
      store;
      active_scope = scope;
      active_dots =
        Some { dot_scope = scope; dot_node = None; dot_usages = []; dot_implicit = None }
    }
  in
  resolve_stmts context program; store

let key = IlluaminateData.Programs.key ~name:__MODULE__ (fun _ _ program -> compute program)
let get_definition var { var_defs; _ } = SVarTbl.find var_defs var
let get_usage var { var_usages; _ } = SVarTbl.find var_usages var

let get_var var { var_defs; var_usages; _ } =
  match SVarTbl.find_opt var_usages var with
  | Some v -> v.var
  | None -> SVarTbl.find var_defs var

let get_dots_definition dot { dots_defs; _ } = TokenTbl.find dots_defs dot
let get_dots_usage dot { dots_usages; _ } = TokenTbl.find dots_usages dot

let get_dots dots { dots_defs; dots_usages; _ } =
  match TokenTbl.find_opt dots_usages dots with
  | Some IllegalDots -> None
  | Some (BoundDots { dots; _ }) -> Some dots
  | None -> Some (TokenTbl.find dots_defs dots)

let globals { globals; _ } = Hashtbl.to_seq_values globals
let get_global { globals; _ } = Hashtbl.find_opt globals

module VarTbl = Hashtbl.Make (struct
  type t = var

  let hash = Hashtbl.hash
  let equal = ( == )
end)
