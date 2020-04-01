open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make Error.Error "syntax:misplaced-dots"

let msg n =
  note ~tag ~span:(Node.span n) "Varargs can only appear as the last argument to a function."

let check { args_args = args; _ } =
  let rec go es =
    let open SepList1 in
    function
    | Cons1 (DotArg t, _, xs) -> go (msg t :: es) xs
    | Cons1 (_, _, xs) -> go es xs
    | Mono _ -> es
  in
  match args with
  | None -> []
  | Some args -> go [] args

let stmt () _ = function
  | LocalFunction { localf_args = args; _ } | AssignFunction { assignf_args = args; _ } ->
      check args
  | _ -> []

let expr () _ = function
  | Fun { fun_args = args; _ } -> check args
  | _ -> []

let linter = make_no_opt ~tags:[ tag ] ~expr ~stmt ()
