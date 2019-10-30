open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let linter =
  let tag = Error.make_tag Error.Warning "var:unbalanced-assign" in
  let too_few =
    [ note ~tag "Right-hand side of assignment has less values than left hand side expects." ]
  and too_many =
    [ note ~tag "Right-hand side of assignment has more values than left hand side expects." ]
  in
  let is_many es =
    match SepList1.last.get es with
    | Dots _ | ECall _ -> true
    | _ -> false
  in
  let stmt () _ = function
    | Assign { assign_vars = vs; assign_vals = es; _ } ->
        let vn = SepList1.length vs and en = SepList1.length es in
        if en > vn then too_many else if en < vn && not (is_many es) then too_few else []
    | Local { local_vars = vs; local_vals = Some (_, es); _ } ->
        let vn = SepList1.length vs and en = SepList1.length es in
        if en > vn then too_many else []
    | _ -> []
  in
  make_no_opt ~tags:[ tag ] ~stmt ()
