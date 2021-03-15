open IlluaminateCore
open Syntax

module Safe = struct
  (* Our handling of binary operators and table indexing is a little restrictive, as we just assume
     they are unsafe (as they may error or have unknown side effects).

     In the future it might be nice to improve this behaviour. For instance, looking up values in
     the environment, or when working with known globals. *)

  let rec expr = function
    | String _ | Number _ | Int _ | MalformedNumber _ | Nil _ | True _ | False _ | Fun _ | Dots _ ->
        true
    | Ref n -> name n
    | (UnOp _ | BinOp _) as e -> (
      match Eval.eval e with
      | RUnknown -> false
      | RNil | RString _ | RBool _ | RNumber _ -> true)
    | Parens { paren_expr; _ } -> expr paren_expr
    | ECall _ -> false
    | Table t -> table t

  and name = function
    | NVar _ -> true
    | NDot _ | NLookup _ -> false

  and table_item = function
    | Array e -> expr e
    | RawPair { value; _ } -> expr value
    | ExprPair { key; value; _ } -> expr key && expr value

  and table { table_body; _ } = List.for_all (fun (x, _) -> table_item x) table_body
end
