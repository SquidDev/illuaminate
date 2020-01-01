open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let linter =
  let tag = Error.Tag.make Error.Error "syntax:malformed-number" in
  let note = [ note ~tag "Malformed number" ] in
  let expr () _ = function
    | MalformedNumber _ -> note
    | _ -> []
  in
  make_no_opt ~tags:[ tag ] ~expr ()
