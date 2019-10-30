open IlluaminateCore.Syntax
open IlluaminateCore
open! Linter

open struct
  module R = IlluaminateSemantics.Resolve
end

let linter =
  let tag = Error.make_tag Error.Warning "var:use-arg" in
  let msg = [ note ~tag "Using implicit vararg variable \"arg\"." ] in
  let name () (context : context) = function
    | NVar (Var name as var) when Node.contents.get name = "arg" -> (
        let resolve = Data.get context.program R.key context.data in
        let var =
          match context.path with
          | Bind :: _ -> R.get_definition var resolve
          | _ -> (R.get_usage var resolve).var
        in
        match var with
        | { R.kind = ImplicitArg _; _ } -> msg
        | _ -> [] )
    | _ -> []
  in
  make_no_opt ~tags:[ tag ] ~name ()
