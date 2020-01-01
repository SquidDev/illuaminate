open Syntax

type repr =
  | RUnknown
  | RNil
  | RBool of bool
  | RNumber of float
  | RString of string
[@@deriving show]

(** Whether this term is truthy, or unknown. *)
let is_truthy = function
  | RUnknown -> None
  | RBool b -> Some b
  | RNil -> Some false
  | RNumber _ | RString _ -> Some true

(** Implicitly convert this value into a number. *)
let as_number = function
  | RNumber x -> Some x
  | RString x -> (
    match float_of_string_opt x with
    | Some x -> Some x
    | None -> None )
  | RUnknown | RNil | RBool _ -> None

(** Implicitly convert this value into a string. *)
let as_string = function
  | RNumber x -> Some (Printf.sprintf "%.14g" x)
  | RString x -> Some x
  | RUnknown | RNil | RBool _ -> None

(** Evaluate this expression.

    [var] should be an (optional) function which evaluates a variable reference. *)
let rec eval ?(var = fun _ -> RUnknown) =
  let bin_op out conv op lhs rhs =
    match (conv lhs, conv rhs) with
    | None, _ | _, None -> RUnknown
    | Some x, Some y -> out (op x y)
  in
  let num_op = bin_op (fun x -> RNumber x) as_number
  and compare op lhs rhs =
    match (lhs, rhs) with
    | RString x, RString y -> RBool (op (String.compare x y))
    | RNumber x, RNumber y ->
        if Float.is_nan x || Float.is_nan y then RBool false else RBool (op (Float.compare x y))
    | _ -> RUnknown
  and eq op lhs rhs =
    let basic x = RBool (op x) in
    match (lhs, rhs) with
    | RUnknown, _ | _, RUnknown -> RUnknown
    | RString x, RString y -> String.equal x y |> basic
    | RNumber x, RNumber y ->
        (if Float.is_nan x || Float.is_nan y then false else Float.equal x y) |> basic
    | RNil, RNil -> basic true
    | RBool x, RBool y -> Bool.equal x y |> basic
    | _ -> basic false
  in
  function
  | Ref (NVar v) -> var v
  | Ref _ | ECall _ | Dots _ | Fun _ | Table _ | MalformedNumber _ -> RUnknown
  | Parens { paren_expr = e; _ } -> eval e
  | Int { lit_value; _ } -> RNumber (float_of_int lit_value)
  | Number { lit_value; _ } -> RNumber lit_value
  | String { lit_value; _ } -> RString lit_value
  | Nil _ -> RNil
  | True _ -> RBool true
  | False _ -> RBool false
  | UnOp { unop_op = op; unop_rhs = e } -> (
      let e = eval e in
      match Node.contents.get op with
      | UnOp.OpLen -> (
        match e with
        | RString x -> RNumber (String.length x |> float_of_int)
        | _ -> RUnknown )
      | UnOp.OpNot -> (
        match is_truthy e with
        | Some b -> RBool (not b)
        | None -> RUnknown )
      | UnOp.OpNeg -> (
        match as_number e with
        | Some x -> RNumber (-.x)
        | None -> RUnknown )
      | UnOp.OpBNot -> RUnknown )
  | BinOp { binop_lhs = lhs; binop_op = op; binop_rhs = rhs } -> (
      let lhs = eval lhs and rhs = eval rhs in
      match Node.contents.get op with
      | BinOp.OpAdd -> num_op ( +. ) lhs rhs
      | BinOp.OpMul -> num_op ( *. ) lhs rhs
      | BinOp.OpSub -> num_op ( -. ) lhs rhs
      | BinOp.OpDiv -> num_op ( /. ) lhs rhs
      | BinOp.OpMod -> num_op mod_float lhs rhs
      | BinOp.OpPow -> num_op ( ** ) lhs rhs
      | BinOp.OpLt -> compare (fun x -> x < 0) lhs rhs
      | BinOp.OpLe -> compare (fun x -> x <= 0) lhs rhs
      | BinOp.OpGt -> compare (fun x -> x > 0) lhs rhs
      | BinOp.OpGe -> compare (fun x -> x >= 0) lhs rhs
      | BinOp.OpConcat -> bin_op (fun x -> RString x) as_string ( ^ ) lhs rhs
      | BinOp.OpAnd -> bin_op (fun x -> RBool x) is_truthy ( && ) lhs rhs
      | BinOp.OpOr -> bin_op (fun x -> RBool x) is_truthy ( || ) lhs rhs
      | BinOp.OpEq -> eq Fun.id lhs rhs
      | BinOp.OpNe -> eq not lhs rhs )
