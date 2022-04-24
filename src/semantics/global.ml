open IlluaminateCore
open Syntax

type t = string list

let parse x = x |> String.split_on_char '.'

let rec of_var_wk store xs var =
  match Resolve.get_var var store with
  | { kind = Global; name; _ } -> Some (name :: xs)
  | { kind = Local _; definitions = [ (_, OfExpr e) ]; _ } -> of_expr_wk store xs e
  | _ -> None

and of_name_wk store xs = function
  | NVar v -> of_var_wk store xs v
  | NDot { tbl; key; _ } -> of_expr_wk store (Node.contents.get key :: xs) tbl
  | NLookup { tbl; key = String { lit_value = key; _ }; _ } -> of_expr_wk store (key :: xs) tbl
  | _ -> None

and of_expr_wk store xs = function
  | Ref name -> of_name_wk store xs name
  | Parens { paren_expr = e; _ } -> of_expr_wk store xs e
  | _ -> None

let of_var store = of_var_wk store []
let of_name store = of_name_wk store []
let of_expr store = of_expr_wk store []
