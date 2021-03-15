open IlluaminateCore
open Syntax

module ArgTbl = Hashtbl.Make (struct
  type t = args

  let equal = ( == )

  let hash = Hashtbl.hash
end)

module StmtTbl = Hashtbl.Make (struct
  type t = stmt

  let equal = ( == )

  let hash = Hashtbl.hash
end)

type edge_source =
  | Jump of stmt
  | Fallthrough

type block_contents =
  | Test of Syntax.expr
  | TestFor of Syntax.stmt
  | Block of Syntax.block
  | LoopEnd of Syntax.stmt

type edge =
  { from_source : edge_source;
    from_block : basic_block;
    to_block : basic_block;
    backwards : bool
  }

and basic_block =
  { block_id : int;
    func : func;
    mutable incoming : edge list;
    mutable outgoing : edge list;
    contents : block_contents
  }

and func =
  { func_id : int;
    entry : basic_block;
    mutable blocks : basic_block list
  }

type pending_stmt =
  | Resolved of basic_block
  | Unresolved of func Lazy.t

type t =
  { mutable next_func : int;
    mutable next_block : int;
    entry_func : func Lazy.t;
    functions : func Lazy.t ArgTbl.t;
    stmts : pending_stmt StmtTbl.t
  }

let build_func t entry_block =
  let rec func = { func_id = t.next_func; entry; blocks = [ entry ] }
  and entry =
    { func; block_id = t.next_block; incoming = []; outgoing = []; contents = Block entry_block }
  in
  t.next_func <- t.next_func + 1;
  t.next_block <- t.next_block + 1;
  let fresh_block contents =
    let block = { func; block_id = t.next_block; incoming = []; outgoing = []; contents } in
    func.blocks <- block :: func.blocks;
    t.next_block <- t.next_block + 1;
    block
  in
  let add_edge from_block to_block from_source backwards =
    let edge = { from_block; to_block; from_source; backwards } in
    from_block.outgoing <- edge :: from_block.outgoing;
    to_block.incoming <- edge :: to_block.incoming
  in
  let add_if stmt from test (truthy, truthy_b) (falsey, falsey_b) =
    let test = Eval.(eval test |> is_truthy) in
    (* Unless we're always true, jump to the false case.*)
    (match test with
    | Some true -> ()
    | _ -> add_edge from falsey (Jump stmt) falsey_b);
    (* Unless we're always false, jump to the true case. *)
    match test with
    | Some false -> ()
    | _ -> add_edge from truthy (Jump stmt) truthy_b
  in
  let rec go loop prev_block = function
    | [] -> prev_block
    | stmt :: stmts as all_stmts ->
        let bb =
          match prev_block with
          | Some bb -> bb
          | None -> fresh_block (Block all_stmts)
        in
        StmtTbl.replace t.stmts stmt (Resolved bb);
        let bb = go_one loop bb stmt stmts in
        go loop bb stmts
  and go_one loop bb stmt rest =
    match stmt with
    | Assign _ | Local _ | LocalFunction _ | AssignFunction _ | SCall _ | Semicolon _ -> Some bb
    | Return _ ->
        (* Return terminates control flow. *)
        None
    | Do { do_body = body; _ } ->
        (* Do just yields whatever the body yields. *)
        go' loop bb body
    | Break _ ->
        (match loop with
        | None -> ()
        | Some continue -> add_edge bb continue (Jump stmt) false);
        None
    | If { if_if; if_elseif; if_else; _ } ->
        let clause { clause_test; clause_body; _ } (next_block, bbs) =
          let test_block = fresh_block (Test clause_test)
          and body_block = fresh_block (Block clause_body) in
          add_if stmt test_block clause_test (body_block, false) (next_block, false);
          let bb = go' loop body_block clause_body in
          (test_block, CCList.cons_maybe bb bbs)
        in
        let continue = fresh_block (Block rest) in
        let next =
          match if_else with
          | None -> (continue, [])
          | Some (_, body) ->
              let block = fresh_block (Block body) in
              add_edge bb block (Jump stmt) false;
              (block, CCOpt.to_list (go' loop block body))
        in
        let entry_block, bbs = List.fold_right clause (if_if :: if_elseif) next in
        add_edge bb entry_block Fallthrough false;
        List.iter (fun x -> add_edge x continue Fallthrough false) bbs;
        Some continue
    | While { while_test = test; while_body = body; _ } ->
        let head = fresh_block (Test test) (* The condition *)
        and loop = fresh_block (Block body) (* The loop body *)
        and loop_end = fresh_block (LoopEnd stmt) (* The end of the loop body *)
        and continue = fresh_block (Block rest) (* Everything after the loop. *) in
        (* The current block jumps to the head, the head jumps to the loop&continuation, and the end
           backwards jumps to the head. *)
        add_edge bb head (Jump stmt) false;
        add_if stmt head test (loop, false) (continue, false);
        add_edge loop_end loop (Jump stmt) true;
        (* All exit nodes of the body jump to the loop end. *)
        go' (Some continue) loop body
        |> Option.iter (fun exit -> add_edge exit loop_end Fallthrough false);
        Some continue
    | Repeat { repeat_test = test; repeat_body = body; _ } ->
        let tail = fresh_block (Test test) (* The condition. *)
        and loop = fresh_block (Block body) (* The loop body *)
        and loop_end = fresh_block (LoopEnd stmt) (* The end of the loop body *)
        and continue = fresh_block (Block rest) (* Everything after the loop. *) in
        (* The current block jumps to the body, the end jumps to the test, and the test jumps to the
           loop&continuation. *)
        add_edge bb loop (Jump stmt) false;
        add_edge loop_end tail (Jump stmt) false;
        add_if stmt tail test (continue, false) (loop, true);
        (* All exit nodes of the body jump to the loop end. *)
        go' (Some continue) loop body
        |> Option.iter (fun exit -> add_edge exit loop_end (Jump stmt) false);
        Some continue
    | ForNum { forn_body = body; _ } | ForIn { forp_body = body; _ } ->
        (* Just the same as while, but without the handling for infinite loops. *)
        let head = fresh_block (TestFor stmt)
        and loop = fresh_block (Block body)
        and loop_end = fresh_block (LoopEnd stmt)
        and continue = fresh_block (Block rest) in
        add_edge bb head (Jump stmt) false;
        add_edge head loop (Jump stmt) false;
        add_edge head continue (Jump stmt) false;
        add_edge loop_end loop (Jump stmt) true;
        go' (Some continue) loop body
        |> Option.iter (fun exit -> add_edge exit loop_end Fallthrough false);
        Some continue
  and go' loop bb = go loop (Some bb) in
  go' None entry entry_block |> ignore;
  func

let analyse _ prog =
  let rec t =
    { next_func = 0;
      next_block = 0;
      functions = ArgTbl.create 4;
      stmts = StmtTbl.create 32;
      entry_func
    }
  and new_func b = lazy (build_func t b)
  and entry_func = lazy (build_func t prog.program) in
  (object
     (* Walks the entire tree, and builds up our maps of statements and functions. This doesn't
        actually compute anything - we'll do that later. *)
     inherit Syntax.iter as super

     val func = entry_func

     method! stmt s =
       StmtTbl.add t.stmts s (Unresolved func);
       match s with
       | LocalFunction { localf_args = args; localf_body = body; _ }
       | AssignFunction { assignf_args = args; assignf_body = body; _ } ->
           let func = new_func body in
           ArgTbl.add t.functions args func; {<func>}#block body
       | _ -> super#stmt s

     method! expr =
       function
       | Fun { fun_args = args; fun_body = body; _ } ->
           let func = new_func body in
           ArgTbl.add t.functions args func; {<func>}#block body
       | e -> super#expr e
  end)
    #program
    prog;
  t

let key = IlluaminateData.Programs.key ~name:__MODULE__ analyse

let get_program t = Lazy.force t.entry_func

let get_func func t = ArgTbl.find t.functions func |> Lazy.force

let get_block stmt t =
  match StmtTbl.find t.stmts stmt with
  | Resolved r -> r
  | Unresolved l -> (
      Lazy.force l |> ignore;
      match StmtTbl.find t.stmts stmt with
      | Resolved r -> r
      | Unresolved _ -> failwith "Should have resolved statement but didn't.")

let write_dot out { entry; blocks; _ } =
  let open Format in
  let get_contents = function
    | Test e -> Format.asprintf "%a" Emit.expr e |> String.trim |> Printf.sprintf "Test <%s>"
    | TestFor s -> Format.asprintf "%a" Emit.stmt s |> String.trim |> Printf.sprintf "TestFor <%s>"
    | Block b -> Format.asprintf "%a" Emit.block b |> String.trim |> Printf.sprintf "Block <%s>"
    | LoopEnd s -> Format.asprintf "%a" Emit.stmt s |> String.trim |> Printf.sprintf "LoopEnd <%s>"
  in
  let get_source = function
    | Jump s -> Format.asprintf "%a" Emit.stmt s |> String.trim |> Printf.sprintf "Jump <%s>"
    | Fallthrough -> "Fallthrough"
  in
  let write_edge { from_source; from_block; to_block; backwards } =
    fprintf out "b%d -> b%d[label=%S,style=%s];@ " from_block.block_id to_block.block_id
      (get_source from_source)
      (if backwards then "dashed" else "solid")
  in
  let write_block { block_id; outgoing; contents; _ } =
    fprintf out "b%d[label=%S,shape=%s];@ " block_id (get_contents contents)
      (if block_id = entry.block_id then "box" else "ellipse");
    List.iter write_edge outgoing;
    ()
  in
  pp_print_string out "digraph G {";
  pp_open_vbox out 2;
  pp_force_newline out ();
  List.iter write_block blocks;
  pp_close_box out ();
  pp_print_string out "}";
  pp_force_newline out ()
