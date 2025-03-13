open IlluaminateCore.Syntax
open IlluaminateCore
module IArray = Illuaminate.IArray
open Linter

(** Missing spaces around binary operators and assignments. *)
let tag_op_space = Error.Tag.make ~attr:[ Default ] ~level:Note "format:op-space"

(** Missing spaces after separators (`,` and `;`). *)
let tag_sep_space = Error.Tag.make ~attr:[ Default ] ~level:Note "format:separator-space"

let has_trailing { Node.trailing_trivia; _ } =
  IArray.length trailing_trivia > 0 && (IArray.first trailing_trivia).kind = Whitespace

let has_leading prev { Node.leading_trivia; span; _ } =
  Span.start_col span = 1
  || (IArray.length leading_trivia > 0 && (IArray.last leading_trivia).kind = Whitespace)
  || has_trailing prev

module Fix = struct
  let after =
    Fixer.fix @@ fun node ->
    let span = Node.span node in
    Ok
      (Node.trailing_trivia.over
         (IArray.push_first (Node.Trivia.make Whitespace " " (Span.start_offset' span)))
         node)

  let around prev node =
    let span = Node.span node in
    let node =
      if has_trailing node then node
      else
        Node.trailing_trivia.over
          (IArray.push_first (Node.Trivia.make Whitespace " " (Span.start_offset' span)))
          node
    in
    let node =
      if has_leading prev node then node
      else
        Node.leading_trivia.over
          (fun x -> IArray.push_last x (Node.Trivia.make Whitespace " " (Span.start_offset' span)))
          node
    in
    node

  let rec table_body = function
    | [] -> []
    | (RawPair ({ ident; eq; _ } as p), c) :: xs ->
        (RawPair { p with eq = around ident eq }, c) :: table_body xs
    | (ExprPair ({ close_k; eq; _ } as p), c) :: xs ->
        (ExprPair { p with eq = around close_k eq }, c) :: table_body xs
    | ((Array _, _) as p) :: xs -> p :: table_body xs

  let expr =
    Fixer.fix @@ function
    | BinOp ({ binop_lhs; binop_op; _ } as op) ->
        Ok (BinOp { op with binop_op = around (Last.expr.get binop_lhs) binop_op })
    | Table x -> Ok (Table { x with table_body = table_body x.table_body })
    | _ -> Error "Unknown expression"

  let stmt =
    Fixer.fix @@ function
    | Assign ({ assign_vars; assign_eq; _ } as stmt) ->
        Ok
          (Assign
             { stmt with
               assign_eq = around (assign_vars |> SepList1.last.get |> Last.name.get) assign_eq
             })
    | ForNum ({ forn_var; forn_eq; _ } as stmt) ->
        Ok (ForNum { stmt with forn_eq = around (Last.var.get forn_var) forn_eq })
    | Local ({ local_vars; local_vals = Some (eql, vs); _ } as stmt) ->
        Ok
          (Local
             { stmt with
               local_vals = Some (around (local_vars |> SepList1.last.get |> Last.var.get) eql, vs)
             })
    | _ -> Error "Unknown statement"
end

module Lint = struct
  let trailing ~r tok =
    if has_trailing tok then ()
    else
      let name = tok |> Node.contents.get |> Token.show in
      r.r ~tag:tag_sep_space ~span:(Node.span tok) ~fix:Fix.after "Expected whitespace after %S."
        name

  let around ~r show fix prev node =
    if has_leading prev node && has_trailing node then ()
    else
      let name = Node.contents.get node |> show in
      r.r ~tag:tag_sep_space ~span:(Node.span node) ~fix "Expected whitespace around %S." name

  let table_body ~r = function
    | RawPair { ident; eq; _ }, _ -> around ~r Token.show Fix.expr ident eq
    | ExprPair { close_k; eq; _ }, _ -> around ~r Token.show Fix.expr close_k eq
    | Array _, _ -> ()

  let token () _ r node =
    match Node.contents.get node with
    | Token.Comma | Token.Semicolon -> trailing ~r node
    | _ -> ()

  let expr () _ r = function
    | BinOp { binop_lhs; binop_op; _ } ->
        around ~r BinOp.show Fix.expr (Last.expr.get binop_lhs) binop_op
    | Table x -> List.iter (table_body ~r) x.table_body
    | _ -> ()

  let stmt () _ r = function
    | Assign { assign_vars; assign_eq; _ } ->
        around ~r Token.show Fix.stmt (assign_vars |> SepList1.last.get |> Last.name.get) assign_eq
    | ForNum { forn_var; forn_eq; _ } ->
        around ~r Token.show Fix.stmt (Last.var.get forn_var) forn_eq
    | Local { local_vars; local_vals = Some (eql, _); _ } ->
        around ~r Token.show Fix.stmt (local_vars |> SepList1.last.get |> Last.var.get) eql
    | _ -> ()
end

let linter =
  let open Lint in
  make_no_opt ~tags:[ tag_op_space; tag_sep_space ] ~token ~expr ~stmt ()
