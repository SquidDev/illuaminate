open IlluaminateCore.Syntax
open IlluaminateCore
open! Linter

open struct
  module C = IlluaminateSemantics.Control
end

let linter =
  let tag_unreach = Error.make_tag Error.Warning "control:unreachable" in
  let tag_loop = Error.make_tag Error.Warning "control:loop-once" in
  let msg_unreach span = [ note ~tag:tag_unreach ~span "Unreachable code" ] in
  let msg_loop span = [ note ~tag:tag_loop ~span "Loop is executed at most once." ] in
  let is_meaningless (edge : C.edge) =
    (* Determine if this edge is meaningless. *)
    match edge.from_block.contents with
    | C.LoopEnd _ -> true
    | _ -> false
  in
  let check_func (func : C.func) =
    let check_block (block : C.basic_block) =
      if block.block_id <> func.entry.block_id && List.for_all is_meaningless block.C.incoming then
        match block.C.contents with
        | Block [] -> []
        | Block (s :: _) -> msg_unreach (Spanned.stmt s)
        | Test e -> msg_unreach (Spanned.expr e)
        | TestFor _ -> []
        | LoopEnd s -> msg_loop (Spanned.stmt s)
      else []
    in
    CCList.flat_map check_block func.blocks
  in
  let check_args (context : context) args =
    let control = Data.get context.program C.key context.data in
    let func = C.get_func args control in
    check_func func
  in
  let stmt () context = function
    | LocalFunction { localf_args = args; _ } | AssignFunction { assignf_args = args; _ } ->
        check_args context args
    | _ -> []
  and expr () context = function
    | Fun { fun_args = args; _ } -> check_args context args
    | _ -> []
  and program () (context : context) (_ : program) =
    let control = Data.get context.program C.key context.data in
    check_func (C.get_program control)
  in
  make_no_opt ~tags:[ tag_unreach; tag_loop ] ~expr ~stmt ~program ()
