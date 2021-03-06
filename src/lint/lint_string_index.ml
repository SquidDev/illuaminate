open IlluaminateCore.Syntax
open IlluaminateCore
open Linter
open Lens

let tag = Error.Tag.make ~attr:[] ~level:Note "syntax:string-index"

let fix =
  Fixer.fix @@ function
  | NLookup { tbl; open_k; key = String { lit_value; lit_node }; close_k } ->
      Ok
        (NDot
           { tbl;
             dot = (Node.contents ^= Token.Dot) @@ open_k;
             key =
               ((Node.contents ^= lit_value)
               % Node.(
                   trailing_trivia %= fun e ->
                   join_trivia e
                     (join_trivia (close_k ^. leading_trivia) (close_k ^. trailing_trivia))))
               @@ lit_node
           })
  | _ -> Error "Not a valid term"

let name () _ r = function
  | NLookup { key = String { lit_value; _ }; _ } when IlluaminateSemantics.Ident.is lit_value ->
      r.r ~tag ~fix "String index can be replaced by identifier."
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~name ()
