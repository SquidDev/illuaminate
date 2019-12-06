open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

(** Missing spaces around binary operators and assignments. *)
let tag_op_space = Error.Tag.make Error.Note "format:op-space"

(** Missing spaces after separators (`,` and `;`). *)
let tag_sep_space = Error.Tag.make Error.Note "format:separator-space"

let has_trailing = function
  | Node.SimpleNode _ -> true
  | Node.Node { trailing_trivia; _ } -> (
    match trailing_trivia with
    | { value = Whitespace ws; _ } :: _ when String.length ws > 0 -> true
    | _ -> false )

let has_leading prev = function
  | Node.SimpleNode _ -> true
  | Node.Node { leading_trivia; span; _ } ->
      span.start_col = 1
      || ( match CCList.last_opt leading_trivia with
         | Some { value = Whitespace ws; _ } when String.length ws > 0 -> true
         | _ -> false )
      || has_trailing prev

module Fix = struct
  let after =
    FixOne
      (fun node ->
        let span = Node.span node in
        Ok (Node.trailing_trivia.over (fun x -> { value = Whitespace " "; span } :: x) node))

  let around prev node =
    let span = Node.span node in
    let node =
      if has_trailing node then node
      else Node.trailing_trivia.over (fun x -> { value = Whitespace " "; span } :: x) node
    in
    let node =
      if has_leading prev node then node
      else Node.leading_trivia.over (fun x -> x @ [ { value = Whitespace " "; span } ]) node
    in
    node

  let rec table = function
    | [] -> []
    | (RawPair ({ ident; eq; _ } as p), c) :: xs ->
        (RawPair { p with eq = around ident eq }, c) :: table xs
    | (ExprPair ({ close_k; eq; _ } as p), c) :: xs ->
        (ExprPair { p with eq = around close_k eq }, c) :: table xs
    | ((Array _, _) as p) :: xs -> p :: table xs

  let args = function
    | CallTable ({ table_body; _ } as t) -> CallTable { t with table_body = table table_body }
    | (CallString _ | CallArgs _) as a -> a

  let call = function
    | Call ({ args = a; _ } as c) -> Call { c with args = args a }
    | Invoke ({ args = a; _ } as c) -> Invoke { c with args = args a }

  let expr =
    FixOne
      (function
      | BinOp ({ binop_lhs; binop_op; _ } as op) ->
          Ok (BinOp { op with binop_op = around (Last.expr.get binop_lhs) binop_op })
      | Table ({ table_body; _ } as t) -> Ok (Table { t with table_body = table table_body })
      | ECall c -> Ok (ECall (call c))
      | _ -> Error "Unknown expression")

  let stmt =
    FixOne
      (function
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
      | SCall c -> Ok (SCall (call c))
      | _ -> Error "Unknown statement")
end

module Lint = struct
  let trailing tok =
    if has_trailing tok then []
    else
      let name = tok |> Node.contents.get |> Token.show in
      [ note ~tag:tag_sep_space ~span:(Node.span tok) ~fix:Fix.after
          (Printf.sprintf "Expected whitespace after %S." name)
      ]

  let around show fix prev node =
    if has_leading prev node && has_trailing node then []
    else
      let name = Node.contents.get node |> show in
      [ note ~tag:tag_sep_space ~span:(Node.span node) ~fix
          (Printf.sprintf "Expected whitespace around %S." name)
      ]

  let rec table fix notes = function
    | [] -> notes
    | (RawPair { ident; eq; _ }, _) :: xs -> table fix (around Token.show fix ident eq @ notes) xs
    | (ExprPair { close_k; eq; _ }, _) :: xs ->
        table fix (around Token.show fix close_k eq @ notes) xs
    | (Array _, _) :: xs -> table fix notes xs

  let args fix = function
    | CallTable { table_body; _ } -> table fix [] table_body
    | CallString _ -> []
    | CallArgs _ -> []

  let call fix = function
    | Call { args = a; _ } -> args fix a
    | Invoke { args = a; _ } -> args fix a

  let token () _ node =
    match Node.contents.get node with
    | Token.Comma | Token.Semicolon -> trailing node
    | _ -> []

  let expr () _ = function
    | BinOp { binop_lhs; binop_op; _ } ->
        around BinOp.show Fix.expr (Last.expr.get binop_lhs) binop_op
    | Table { table_body; _ } -> table Fix.expr [] table_body
    | ECall c -> call Fix.expr c
    | _ -> []

  let stmt () _ = function
    | Assign { assign_vars; assign_eq; _ } ->
        around Token.show Fix.stmt (assign_vars |> SepList1.last.get |> Last.name.get) assign_eq
    | ForNum { forn_var; forn_eq; _ } -> around Token.show Fix.stmt (Last.var.get forn_var) forn_eq
    | Local { local_vars; local_vals = Some (eql, _); _ } ->
        around Token.show Fix.stmt (local_vars |> SepList1.last.get |> Last.var.get) eql
    | SCall c -> call Fix.stmt c
    | _ -> []
end

let linter =
  let open Lint in
  make_no_opt ~tags:[ tag_op_space; tag_sep_space ] ~token ~expr ~stmt ()
