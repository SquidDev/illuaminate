open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "var:arg-arg"

let check ~r { args_args = args; _ } =
  let go = function
    | NamedArg (Var n) when Node.contents.get n = "arg" ->
        r.r ~tag ~span:(Node.span n) "Argument named \"arg\"."
    | _ -> ()
  in
  SepList0.iter go args

let stmt () _ r = function
  | LocalFunction { localf_args = args; _ } | AssignFunction { assignf_args = args; _ } ->
      check ~r args
  | _ -> ()

let expr () _ r = function
  | Fun { fun_args = args; _ } -> check ~r args
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~expr ~stmt ()
