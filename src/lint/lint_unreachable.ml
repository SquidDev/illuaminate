open IlluaminateCore.Syntax
open IlluaminateCore
open IlluaminateSemantics
open! Linter
module C = Control

let tag_unreach =
  (* TODO: Make this use "Unused" too. *)
  Error.Tag.make ~attr:[ Default ] ~level:Warning "control:unreachable"

let tag_loop = Error.Tag.make ~attr:[ Default ] ~level:Warning "control:loop-once"

let msg_unreach ~r span = r.r ~tag:tag_unreach ~span "Unreachable code"

let msg_loop ~r span = r.r ~tag:tag_loop ~span "Loop is executed at most once."

let check_func ~r (func : C.func) =
  let check_block (block : C.basic_block) =
    if block.block_id <> func.entry.block_id && CCList.is_empty block.incoming then
      (* FIXME: This test really isn't perfect.

         For instance, `while true do break end end` will report a loop which only runs once, but
         `while true do do break end print("Hello") end will report unreachable code. Ideally it'd
         report both.

         The inverse problem occurs with repeat/until loops, where the test will not be marked
         unreachable, but the loop is marked as only being iterable once.*)
      match block.contents with
      | Block [] -> ()
      | Block (s :: _) -> msg_unreach ~r (Spanned.stmt s)
      | Test e -> msg_unreach ~r (Spanned.expr e)
      | TestFor s -> msg_unreach ~r (Spanned.stmt s)
      | LoopEnd s -> msg_loop ~r (Spanned.stmt s)
  in
  List.iter check_block func.blocks

let check_args (context : context) ~r args =
  let control = IlluaminateData.need context.data C.key context.program in
  let func = C.get_func args control in
  check_func ~r func

let stmt () context r = function
  | LocalFunction { localf_args = args; _ } | AssignFunction { assignf_args = args; _ } ->
      check_args context ~r args
  | _ -> ()

let expr () context r = function
  | Fun { fun_args = args; _ } -> check_args context ~r args
  | _ -> ()

let program () (context : context) r (_ : program) =
  let control = IlluaminateData.need context.data C.key context.program in
  check_func ~r (C.get_program control)

let linter = make_no_opt ~tags:[ tag_unreach; tag_loop ] ~expr ~stmt ~program ()
