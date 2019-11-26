open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let linter =
  let tag = Error.Tag.make Error.Warning "var:use-discard" in
  let msg = [ note ~tag "Using \"discard\" variable `_`." ] in
  let expr () _ = function
    | Ref (NVar (Var name)) when Node.contents.get name = "_" -> msg
    | _ -> []
  in
  make_no_opt ~tags:[ tag ] ~expr ()
