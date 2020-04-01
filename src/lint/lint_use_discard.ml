open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make Error.Warning "var:use-discard"

let msg = [ note ~tag "Using \"discard\" variable `_`." ]

let expr () _ = function
  | Ref (NVar (Var name)) when Node.contents.get name = "_" -> msg
  | _ -> []

let linter = make_no_opt ~tags:[ tag ] ~expr ()
