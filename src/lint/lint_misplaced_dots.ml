open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make ~attr:[ Default ] ~level:Error "syntax:misplaced-dots"

let check ~r { args_args = args; _ } =
  let rec go =
    let open SepList1 in
    function
    | Cons1 (DotArg t, _, xs) ->
        r.r ~tag ~span:(Node.span t) "Varargs can only appear as the last argument to a function.";
        go xs
    | Cons1 (_, _, xs) -> go xs
    | Mono _ -> ()
  in
  Option.iter go args

let stmt () _ r = function
  | LocalFunction { localf_args = args; _ } | AssignFunction { assignf_args = args; _ } ->
      check ~r args
  | _ -> ()

let expr () _ r = function
  | Fun { fun_args = args; _ } -> check ~r args
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~expr ~stmt ()
