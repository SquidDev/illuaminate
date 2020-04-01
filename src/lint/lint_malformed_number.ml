open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make Error.Error "syntax:malformed-number"

let expr () _ = function
  | MalformedNumber _ -> [ note ~tag "Malformed number" ]
  | _ -> []

let linter = make_no_opt ~tags:[ tag ] ~expr ()
