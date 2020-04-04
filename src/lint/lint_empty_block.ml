open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

module Opt = struct
  type t = { allow_empty_if : bool }

  let parse =
    let open IlluaminateConfig in
    let open Term in
    let term =
      let+ allow_empty_if =
        field ~name:"allow-empty-if" ~comment:"Allow empty if or elseif blocks." ~default:false
          Converter.bool
      in
      { allow_empty_if }
    in
    Category.add term category
end

let tag_do = Error.Tag.make ~attr:[ Default; Unused ] ~level:Warning "syntax:empty-do"

let tag_if = Error.Tag.make ~attr:[ Default ] ~level:Warning "syntax:empty-if"

let msg_do =
  let fix =
    Fixer.block @@ function
    | Do { do_body = []; _ } -> Ok []
    | _ -> Error "Expected empty `do' block."
  in
  [ note ~fix ~tag:tag_do "Empty do statement" ]

let fix_else =
  Fixer.fix @@ function
  | If ({ if_else = Some (_, []); _ } as ifs) -> Ok (If { ifs with if_else = None })
  | _ -> Error "Expected empty `else' clause."

let clause_span { clause_if; clause_then; _ } =
  Span.of_span2 (Node.span clause_if) (Node.span clause_then)

let stmt { Opt.allow_empty_if } _ = function
  | Do { do_body = []; _ } -> msg_do
  | If { if_if; if_elseif; if_else; _ } ->
      let clauses =
        List.fold_left
          (fun xs ({ clause_body; _ } as c) ->
            ("elseif", clause_body, clause_span c, Fixer.none) :: xs)
          [ ("if", if_if.clause_body, clause_span if_if, Fixer.none) ]
          if_elseif
      in
      let clauses =
        match if_else with
        | Some (tok, body) -> ("else", body, Node.span tok, fix_else) :: clauses
        | None -> clauses
      in
      if allow_empty_if then
        match clauses with
        | (name, [], span, fix) :: _ -> [ note ~span ~fix ~tag:tag_if "Empty %s clause" name ]
        | _ -> []
      else
        List.filter_map
          (function
            | name, [], span, fix -> Some (note ~span ~fix ~tag:tag_if "Empty %s clause" name)
            | _ -> None)
          clauses
  | _ -> []

let linter = make ~options:Opt.parse ~tags:[ tag_do; tag_if ] ~stmt ()
