open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "control:for-num"

let count_nowhere r =
  r.r ~tag "This loop has the same start and stop point, and so will only execute once."

let count_nothing r = r.r ~tag "Numeric for loop has a step of 0. It will never progress."

let count_up ~start ~limit r =
  r.r ~tag "Numeric for loop counts up from %.14g to %.14g, but has a negative step." start limit

let count_down ~start ~limit r =
  r.r ~tag "Numeric for loop counts down from %.14g to %.14g, but has a non-negative step." start
    limit

let count_for ~limit r =
  r.r ~tag "Numeric for loop counts down from #(expr) to %.14g, but has a non-negative step." limit

let rec unwrap = function
  | Parens { paren_expr = e; _ } -> unwrap e
  | e -> e

let stmt () _ r = function
  | ForNum { forn_start; forn_limit; forn_step; _ } -> (
      let start = Eval.(eval forn_start |> as_number)
      and limit = Eval.(eval forn_limit |> as_number)
      and step =
        match forn_step with
        | None -> Some 1.0
        | Some (_, x) -> Eval.(eval x |> as_number)
      in
      match (start, limit, step) with
      | Some start, Some limit, _ when Float.equal start limit -> count_nowhere r
      | _, _, Some 0.0 -> count_nothing r
      (* If we've a known start/stop/step, then we can check the loop will terminate *)
      | Some start, Some limit, Some step when limit > start && step < 0.0 ->
          count_up ~start ~limit r
      | Some start, Some limit, Some step when limit < start && step > 0.0 ->
          count_down ~start ~limit r
      (* If we're counting down from a length to 1, then assume the step should be negative. *)
      | None, Some limit, Some step when limit <= 1.0 && step > 0.0 -> (
        match unwrap forn_start with
        | UnOp { unop_op = op; _ } when Node.contents.get op = UnOp.OpLen -> count_for ~limit r
        | _ -> () )
      | _ -> () )
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~stmt ()
