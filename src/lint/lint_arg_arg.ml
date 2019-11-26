open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let linter =
  let tag = Error.Tag.make Error.Warning "var:arg-arg" in
  let msg n = note ~tag ~span:(Node.span n) "Argument named \"arg\"." in
  let check { args_args = args; _ } =
    let go es = function
      | NamedArg (Var name) when Node.contents.get name = "arg" -> msg name :: es
      | _ -> es
    in
    SepList0.fold_left go [] args
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
