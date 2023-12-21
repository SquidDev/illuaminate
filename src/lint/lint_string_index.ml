open IlluaminateCore.Syntax
open IlluaminateCore
open Linter
open Lens

let tag = Error.Tag.make ~attr:[] ~level:Note "syntax:string-index"

let fix =
  Fixer.fix @@ function
  | NLookup { tbl; open_k; key = String node; close_k } ->
      Ok
        (NDot
           { tbl;
             dot = (Node.contents ^= Token.Dot) @@ open_k;
             key =
               (Node.contents
               %= (Result.get_ok % Illuaminate.Syntax.Literal.String.parse_value)
               % Node.(
                   trailing_trivia %= fun e ->
                   join_trivia e
                     (join_trivia (close_k ^. leading_trivia) (close_k ^. trailing_trivia))))
               @@ node
           })
  | _ -> Error "Not a valid term"

let name () _ r = function
  | NLookup { key = String node; _ } -> (
    match Node.contents.get node |> Illuaminate.Syntax.Literal.String.parse_value with
    | Ok name when Illuaminate.Syntax.Ident.is name ->
        r.r ~tag ~fix "String index can be replaced by identifier."
    | _ -> ())
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~name ()
