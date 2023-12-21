open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make ~attr:[ Default ] ~level:Error "syntax:malformed-number"

let expr () _ r = function
  | Number n when Node.contents.get n |> Illuaminate.Syntax.Literal.Number.parse |> Result.is_error
    -> r.r ~tag "Malformed number"
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~expr ()
