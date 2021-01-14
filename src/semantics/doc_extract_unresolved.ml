open IlluaminateCore
module StringMap = Map.Make (String)
open Syntax
open! Doc_syntax
open Doc_extract_helpers

open struct
  module R = Resolve
  module C = Doc_comment
  module P = Doc_parser.Data
  module VarTbl = R.VarTbl
  module D = IlluaminateData
end

module CommentCollection = Hashtbl.Make (struct
  type t = C.comment

  let equal = ( == )

  let hash = Hashtbl.hash
end)

type state =
  { errs : Error.t;
    unused_comments : unit CommentCollection.t;
    comments : P.t;
    resolve : R.t;
    vars : value documented ref VarTbl.t;
    globals : value documented ref;
    mutable export : value documented ref option;
    mutable types : type_info documented StringMap.t
  }

let report state = Error.report state.errs

type result =
  | Named of page documented
  | Unnamed of
      { file : Span.filename;
        body : value documented;
        mod_types : type_info documented list;
        mod_kind : module_kind;
        mod_namespace : Namespace.t
      }

let mk_page ~mod_name ~mod_namespace ~mod_kind ~mod_contents ~mod_types =
  { page_id = mod_name;
    page_title = mod_name;
    page_namespace = mod_namespace;
    page_contents = Module { mod_contents; mod_types; mod_kind }
  }

module Infer = struct
  (** Construct a simple documented node, which has no additional information. *)
  let simple_documented descriptor definition =
    { description = None;
      descriptor;
      definition;
      examples = [];
      see = [];
      local = false;
      export = false;
      deprecated = None;
      custom_source = None
    }

  (** Annotate a value with documentation comments. *)
  let document state ~before ~after value =
    let add_doc comment value =
      match comment with
      | Some comment when CommentCollection.mem state.unused_comments comment ->
          CommentCollection.remove state.unused_comments comment;
          Value.get_documented ~report:(report state) comment
          |> Merge.doc_value ~errs:state.errs value
      | _ -> value
    in
    value |> add_doc before |> add_doc after

  (** Filter attached documentation comments and annotate a value with them. *)
  let documenting state ~before ~after (span : Span.t) value : value documented =
    (* Limit comments to ones directly before/after the current node and appropriately aligned with
       it. *)
    let start_line = Span.start_line span
    and finish_line = Span.finish_line span
    and start_col = Span.start_col.get span in
    let before =
      match before with
      | ({ C.source; _ } as c) :: _
        when Span.start_line source = start_line
             || (Span.finish_line source = start_line - 1 && Span.start_col.get source = start_col)
        ->
          Some c
      | _ -> None
    and after =
      match after with
      | ({ C.source; _ } as c) :: _
        when Span.finish_line source = finish_line
             || (Span.start_line source = finish_line + 1 && Span.start_col.get source = start_col)
        ->
          Some c
      | _ -> None
    in
    document state ~before ~after value

  (** Annotate a value with documentation comments taken from a pair of nodes. *)
  let document_with state ~before ~after span value : value documented =
    let before, _ = P.comment before state.comments in
    let _, after = P.comment after state.comments in
    documenting state ~before ~after span value

  (** Annotate a value with documentation comments taken from a statement. *)
  let document_stmt state stmt value : value documented =
    document_with state ~before:(First.stmt.get stmt) ~after:(Last.stmt.get stmt)
      (Spanned.stmt stmt) value

  (** Add a {!R.var} to the current scope. *)
  let add_resolved_var state var def =
    let update_info def =
      (* Add this term to the type map. Oh boy, this is almost definitely wrong *)
      ( match !def with
      | { descriptor = Type ({ type_name; _ } as ty); _ } as def ->
          state.types <- StringMap.add type_name { def with descriptor = ty } state.types
      | _ -> () );
      (* Export this variable if required. *)
      if !def.export then state.export <- Some def
    in
    ( if not (VarTbl.mem state.vars var) then
      match var with
      | { kind = Global; name = "_ENV"; _ } -> VarTbl.add state.vars var state.globals
      | { kind = Global; _ } -> (
        match !(state.globals) with
        | { descriptor = Table fs; _ } as globals ->
            state.globals := { globals with descriptor = Table (fs @ [ (var.name, def) ]) }
        | _ -> () )
      | _ -> () );
    match VarTbl.find_opt state.vars var with
    | None ->
        let def = ref def in
        VarTbl.add state.vars var def; update_info def
    | Some existing ->
        existing := Merge.doc_value ~errs:state.errs !existing def;
        update_info existing

  (** Add a {!var} to the current scope. *)
  let add_var state var def = add_resolved_var state (R.get_definition var state.resolve) def

  (** Add a {!name} to the current scope. The definition is lazy, as it's not guaranteed we do
      anything with it.*)
  let add_name state var (def : _ Lazy.t) : unit =
    let rec go def = function
      | NVar var ->
          let var = R.get_var var state.resolve in
          add_resolved_var state var (Lazy.force def)
      | NDot { tbl = Ref tbl; key; _ } ->
          Fun.flip go tbl
            ( lazy
              (let def = Lazy.force def in
               simple_documented (Doc_syntax.Table [ (Node.contents.get key, def) ]) def.definition)
              )
      | NLookup { tbl = Ref tbl; key = String { lit_value; _ }; _ } ->
          Fun.flip go tbl
            ( lazy
              (let def = Lazy.force def in
               simple_documented (Doc_syntax.Table [ (lit_value, def) ]) def.definition) )
      | _ -> ()
    in
    match var with
    | NVar var ->
        let var = R.get_definition var state.resolve in
        add_resolved_var state var (Lazy.force def)
    | NDot _ | NLookup _ -> go def var

  (** Add a {!function_name} to the current scope. *)
  let rec add_fname state var def =
    match var with
    | FVar var -> add_resolved_var state (R.get_var var state.resolve) def
    | FDot { tbl; field; _ } | FSelf { tbl; meth = field; _ } ->
        simple_documented (Doc_syntax.Table [ (Node.contents.get field, def) ]) def.definition
        |> add_fname state tbl

  (** Infer the documented type of an expression. *)
  let rec infer_expr state expr =
    let simp x = simple_documented x (Spanned.expr expr) in
    match expr with
    | Fun { fun_args; fun_body; _ } -> infer_fun state fun_args fun_body |> simp
    | Table { table_body; _ } ->
        let fields = infer_table state table_body in
        Doc_syntax.Table fields |> simp
    | String { lit_value; _ } ->
        Expr
          { ty = Type_syntax.Builtin.string;
            value =
              ( if String.length lit_value < 32 then Printf.sprintf "%S" lit_value |> Option.some
              else None )
          }
        |> simp
    | Number { lit_node; _ } | Int { lit_node; _ } | MalformedNumber lit_node ->
        Expr { ty = Type_syntax.Builtin.number; value = Node.contents.get lit_node |> Option.some }
        |> simp
    | True _ -> Expr { ty = Type_syntax.Builtin.boolean; value = Some "true" } |> simp
    | False _ -> Expr { ty = Type_syntax.Builtin.boolean; value = Some "false" } |> simp
    | Nil _ -> Expr { ty = Type.NilTy; value = Some "nil" } |> simp
    | Ref v -> (
      match infer_name state v with
      | None -> simp Unknown
      | Some x -> !x )
    | Parens { paren_expr; _ } -> infer_expr state paren_expr
    (* Visit children and return nothing. *)
    | Dots _ -> simp Unknown
    | ECall c -> visit_call state c; simp Unknown
    | UnOp { unop_rhs; _ } -> visit_expr state unop_rhs; simp Unknown
    | BinOp { binop_lhs; binop_rhs; _ } ->
        visit_expr state binop_lhs; visit_expr state binop_rhs; simp Unknown

  and visit_expr state v : unit = infer_expr state v |> ignore

  and visit_call state =
    let visit_args = function
      | CallArgs { args; _ } -> SepList0.iter (visit_expr state) args
      | CallString _ -> ()
      | CallTable { table_body; _ } -> infer_table state table_body |> ignore
    in
    function
    | Call { fn = e; args } | Invoke { obj = e; args; _ } -> visit_expr state e; visit_args args

  and infer_var state v : value documented ref option =
    match R.get_var v state.resolve with
    | { kind = Global; name = "_ENV"; _ } -> Some state.globals
    | var -> VarTbl.find_opt state.vars var

  and infer_name state : name -> value documented ref option = function
    | NVar v -> infer_var state v
    (* TODO: Would be good to do NDot, somehow. *)
    | NDot { tbl; key; _ } -> (
      match infer_expr state tbl with
      | { descriptor = Table fs; _ } -> List.assoc_opt (Node.contents.get key) fs |> Option.map ref
      | { descriptor = Type { type_members; _ }; _ } ->
          List.find_opt (fun x -> Node.contents.get key = x.member_name) type_members
          |> Option.map (fun x -> ref x.member_value)
      | _ -> None )
    | NLookup { tbl; key; _ } -> visit_expr state tbl; visit_expr state key; None

  (** Get a documented table entry *)
  and infer_table state = function
    | [] -> []
    | ((RawPair { ident; value; _ } as p), sep) :: xs ->
        let before = First.table_item.get p
        and after =
          match sep with
          | None -> Last.table_item.get p
          | Some sep -> sep
        in
        let value =
          infer_expr state value |> document_with state ~before ~after (Spanned.table_item p)
        in
        (Node.contents.get ident, value) :: infer_table state xs
    (* For now, we just skip all other table items. In the future it might be worth adding type
       hints or something. *)
    | (Array e, _) :: xs -> visit_expr state e; infer_table state xs
    | (ExprPair { key; value; _ }, _) :: xs ->
        visit_expr state key; visit_expr state value; infer_table state xs

  (** Get the descriptor for a list of arguments *)
  and infer_fun state ?(has_self = false) args body =
    let get_name = function
      | DotArg _ -> "..."
      | NamedArg (Var v) -> Node.contents.get v
    in
    let get_arg arg =
      { arg_name = get_name arg; arg_opt = Required; arg_type = None; arg_description = None }
    in
    infer_stmts state body |> ignore;
    Function { args = [ SepList0.map' get_arg args.args_args ]; rets = []; throws = []; has_self }

  (** Merge a statement's definition with an (optional) doc comment and apply it to the current
      scope. *)
  and infer_stmt state (node : stmt) =
    (* Oh goodness, this is horrible. We really need an actual data-flow algorithm here, so we can
       handle various Lua idioms correctly. This works (albeit weirdly) for now. *)
    match node with
    | Local { local_vars = Mono var; local_vals = Some (_, Mono (Ref (NVar def_var) as def)); _ } as
      s ->
        ( match infer_var state def_var with
        | None -> infer_expr state def |> document_stmt state s |> add_var state var
        | Some def ->
            def := document_stmt state s !def;
            let var = R.get_definition var state.resolve in
            VarTbl.add state.vars var def );
        None
    | Local { local_vars = Mono var; local_vals = Some (_, Mono def); _ } as s ->
        infer_expr state def |> document_stmt state s |> add_var state var;
        None
    | Local { local_vars = Mono var; local_vals = None; _ } as s ->
        simple_documented Unknown (Spanned.stmt s) |> document_stmt state s |> add_var state var;
        None
    | LocalFunction { localf_var; localf_args; localf_body; _ } as s ->
        simple_documented (infer_fun state localf_args localf_body) (Spanned.stmt s)
        |> document_stmt state s |> add_var state localf_var;
        None
    | AssignFunction { assignf_name; assignf_args; assignf_body; _ } as s ->
        let has_self =
          match assignf_name with
          | FVar _ | FDot _ -> false
          | FSelf _ -> true
        in
        simple_documented (infer_fun state ~has_self assignf_args assignf_body) (Spanned.stmt node)
        |> document_stmt state s |> add_fname state assignf_name;
        None
    | Assign { assign_vars = Mono var; assign_vals = Mono def; _ } as s ->
        lazy (infer_expr state def |> document_stmt state s) |> add_name state var;
        None
    | Return { return_vals = Some (Mono expr); _ } as s ->
        infer_expr state expr |> document_stmt state s |> Option.some
    (* Visit children and return nothing. *)
    | SCall c -> visit_call state c; None
    | Break _ | Semicolon _ -> None
    | stmt ->
        (* For any other node, just visit any child expressions/blocks. This is very lazy, but
           works. If we ever get a more sane type inference algorithm, obviously this'll be
           revisited. *)
        let c =
          object
            inherit Syntax.iter

            method! block x = infer_stmts state x |> ignore

            method! expr x = infer_expr state x |> ignore
          end
        in
        c#stmt stmt; None

  and infer_stmts state = function
    | [] -> None
    | node :: xs ->
        let docs = infer_stmt state node and rest = infer_stmts state xs in
        CCOpt.( <+> ) docs rest

  let extract_module data program =
    let errs = Error.make () in
    let comments = D.need data P.key program in
    let resolve = D.need data R.key program in
    let env = ref (simple_documented (Doc_syntax.Table []) (Spanned.program program)) in
    let state =
      { errs;
        unused_comments = CommentCollection.create 16;
        comments;
        resolve;
        vars = VarTbl.create 16;
        globals = env;
        export = None;
        types = StringMap.empty
      }
    in
    P.comments comments |> List.iter (fun c -> CommentCollection.add state.unused_comments c ());

    let mod_kind, mod_namespace, body =
      match infer_stmts state program.program with
      | Some x -> (MKLibrary, Namespace.library, x)
      | None -> (MKModule, Namespace.module_, !env)
    in
    let body = Option.fold ~none:body ~some:(fun x -> !x) state.export in
    let body = { body with descriptor = DropLocal.value body.descriptor } in
    let module_comment =
      match P.comment (First.program.get program) state.comments |> fst |> CCList.last_opt with
      | Some ({ source; _ } as c)
        when Span.start_col.get source = 1
             && Span.start_line source = 1
             && CommentCollection.mem state.unused_comments c ->
          CommentCollection.remove state.unused_comments c;
          Some c
      | _ -> None
    in
    let mod_types = StringMap.bindings state.types |> List.map snd |> DropLocal.mod_types in
    let result =
      match module_comment with
      | Some
          ( { module_info = Some { value = { mod_name; mod_kind = k; mod_namespace = ns; _ }; _ };
              _
            } as comment ) ->
          let merge pos implicit body =
            let mod_contents = Merge.value ~errs:state.errs pos implicit body in
            mk_page ~mod_name ~mod_contents ~mod_types
              ~mod_kind:(Option.value ~default:mod_kind k)
              ~mod_namespace:(Option.value ~default:mod_namespace ns)
          in
          Named (Value.get_documented ~report:(report state) comment |> Merge.documented merge body)
      | Some ({ module_info = None; _ } as comment) ->
          let body =
            Value.get_documented ~report:(report state) comment
            |> Merge.doc_value ~errs:state.errs body
          in
          Unnamed
            { body;
              mod_types;
              file = Spanned.program program |> Span.filename;
              mod_kind;
              mod_namespace
            }
      | None ->
          Unnamed
            { body;
              mod_types;
              file = Spanned.program program |> Span.filename;
              mod_kind;
              mod_namespace
            }
    in
    (state, result)

  let key = D.Programs.key ~name:(__MODULE__ ^ ".Infer") extract_module
end

let get_unresolved_module data program =
  match D.need data Infer.key program |> snd with
  | Named m -> Some m
  | Unnamed { file = { path = None; _ }; _ } -> None
  | Unnamed { file = { path = Some path; _ }; body; mod_types; mod_kind; mod_namespace } ->
      D.need data D.Programs.Context.key (Spanned.program program |> Span.filename)
      |> Doc_extract_config.guess_module path
      |> Option.map @@ fun mod_name ->
         { body with
           descriptor =
             mk_page ~mod_name ~mod_namespace ~mod_types ~mod_kind ~mod_contents:body.descriptor
         }

let unresolved_module = D.Programs.key ~name:(__MODULE__ ^ ".unresolved") get_unresolved_module
