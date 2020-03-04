open IlluaminateCore.Syntax
open IlluaminateCore
open IlluaminateSemantics
open! Linter

open struct
  module R = Resolve
end

let linter =
  let tag = Error.Tag.make Error.Warning "var:use-arg" in
  let msg = [ note ~tag "Using implicit vararg variable \"arg\"." ] in
  let name () (context : context) = function
    | NVar (Var name as var) when Node.contents.get name = "arg" -> (
        let resolve = IlluaminateData.get context.data R.key context.program in
        let var = R.get_var var resolve in
        match var with
        | { R.kind = ImplicitArg _; _ } -> msg
        | _ -> [] )
    | _ -> []
  in
  make_no_opt ~tags:[ tag ] ~name ()
