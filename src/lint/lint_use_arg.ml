open IlluaminateCore.Syntax
open IlluaminateCore
open IlluaminateSemantics
open! Linter
module R = Resolve

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "var:use-arg"

let name () (context : context) r = function
  | NVar (Var name as var) when Node.contents.get name = "arg" -> (
      let resolve = IlluaminateData.need context.data R.key context.program in
      let var = R.get_var var resolve in
      match var with
      | { R.kind = ImplicitArg _; _ } -> r.r ~tag "Using implicit vararg variable \"arg\"."
      | _ -> () )
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~name ()
