open IlluaminateCore.Syntax
open IlluaminateCore
open Linter
open Lens

let linter =
  let tag = Error.make_tag Error.Warning "syntax:redundant-semicolon" in
  let fix =
    FixBlock
      (function
      | Semicolon _ -> Ok []
      | _ -> Error "Expected semicolon")
  in
  let msg = [ note ~fix ~tag "Redundant semicolon." ] in
  let stmt () context stmt =
    match (context.path, stmt) with
    | Block bs :: _, (Semicolon _ as semi) ->
        let rec go before = function
          | [] -> invalid_arg "Not within the list"
          | x :: xs when x == semi -> (before, xs)
          | x :: xs -> go (x :: before) xs
        in
        let pointless =
          match go [] bs with
          | [], _ | _, [] -> true
          | before :: _, after :: _ -> (
              let before = before ^. (Last.stmt -| Node.contents)
              and after = after ^. (First.stmt -| Node.contents) in
              match (before, after) with
              | Token.CParen, (Token.OParen | Token.String _ | Token.OBrace) -> false
              | _ -> true )
        in
        (* TODO: We shouldn't offer a fix if we've a chain of semicolons, and at least one is
           required. For instance: [(f)() ;;; f()] should offer fixes on all but the first. *)
        if pointless then msg else []
    | _ -> []
  in
  make_no_opt ~tags:[ tag ] ~stmt ()
