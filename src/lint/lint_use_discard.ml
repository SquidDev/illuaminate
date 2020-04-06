open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "var:use-discard"

let expr () _ r = function
  | Ref (NVar (Var name)) when Node.contents.get name = "_" ->
      r.r ~tag "Using \"discard\" variable `_`."
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~expr ()
