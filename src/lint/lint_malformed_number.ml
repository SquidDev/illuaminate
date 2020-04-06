open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make ~attr:[ Default ] ~level:Error "syntax:malformed-number"

let expr () _ r = function
  | MalformedNumber _ -> r.r ~tag "Malformed number"
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~expr ()
