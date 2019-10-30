open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let linter =
  let tag = Error.make_tag Error.Error "syntax:misplaced-dots" in
  let msg n =
    note ~tag ~span:(Node.span n) "Varargs can only appear as the last argument to a function."
  in
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
  in
  let stmt () _ = function
    | LocalFunction { localf_args = args; _ } | AssignFunction { assignf_args = args; _ } ->
        check args
    | _ -> []
  and expr () _ = function
    | Fun { fun_args = args; _ } -> check args
    | _ -> []
  in
  make_no_opt ~tags:[ tag ] ~expr ~stmt ()
