open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let expr () _ r = function
  | Number n when Node.contents.get n |> Illuaminate.Syntax.Literal.Number.parse |> Result.is_error
    ->
      r.x
        (Illuaminate.Error.simple ~code:"syntax:malformed-number" ~severity:Error
           (Node.span n |> Span.to_error_position)
           (fun f -> f "Malformed number"))
  | _ -> ()

let linter = make_no_opt ~tags:[] ~expr ()
