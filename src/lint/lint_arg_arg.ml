open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "var:arg-arg"

let msg n = note ~tag ~span:(Node.span n) "Argument named \"arg\"."

let check { args_args = args; _ } =
  let go es = function
    | NamedArg (Var name) when Node.contents.get name = "arg" -> msg name :: es
    | _ -> es
  in
  SepList0.fold_left go [] args

let stmt () _ = function
  | LocalFunction { localf_args = args; _ } | AssignFunction { assignf_args = args; _ } ->
      check args
  | _ -> []

let expr () _ = function
  | Fun { fun_args = args; _ } -> check args
  | _ -> []

let linter = make_no_opt ~tags:[ tag ] ~expr ~stmt ()
