open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let linter =
  let tag = Error.make_tag Error.Warning "control:for-num" in
  let count_nowhere =
    [ note ~tag "This loop has the same start and stop point, and so will only execute once." ]
  and count_nothing = [ note ~tag "Numeric for loop has a step of 0. It will never progress." ]
  and count_up ~start ~limit =
    [ Printf.sprintf "Numeric for loop counts up from %.14g to %.14g, but has a negative step."
        start limit
      |> note ~tag
    ]
  and count_down ~start ~limit =
    [ Printf.sprintf
        "Numeric for loop counts down from %.14g to %.14g, but has a non-negative step." start limit
      |> note ~tag
    ]
  and count_for ~limit =
    [ Printf.sprintf
        "Numeric for loop counts down from #(expr) to %.14g, but has a non-negative step." limit
      |> note ~tag
    ]
  in
  let rec unwrap = function
    | Parens { paren_expr = e; _ } -> unwrap e
    | e -> e
  in
  let stmt () _ = function
    | ForNum { forn_start; forn_limit; forn_step; _ } -> (
        let start = Eval.(eval forn_start |> as_number)
        and limit = Eval.(eval forn_limit |> as_number)
        and step =
          match forn_step with
          | None -> Some 1.0
          | Some (_, x) -> Eval.(eval x |> as_number)
        in
        match (start, limit, step) with
        | Some start, Some limit, _ when Float.equal start limit -> count_nowhere
        | _, _, Some 0.0 -> count_nothing
        (* If we've a known start/stop/step, then we can check the loop will terminate *)
        | Some start, Some limit, Some step when limit > start && step < 0.0 ->
            count_up ~start ~limit
        | Some start, Some limit, Some step when limit < start && step > 0.0 ->
            count_down ~start ~limit
        (* If we're counting down from a length to 1, then assume the step should be negative. *)
        | None, Some limit, Some step when limit <= 1.0 && step > 0.0 -> (
          match unwrap forn_start with
          | UnOp { unop_op = op; _ } when Node.contents.get op = UnOp.OpLen -> count_for ~limit
          | _ -> [] )
        | _ -> [] )
    | _ -> []
  in
  make_no_opt ~tags:[ tag ] ~stmt ()
