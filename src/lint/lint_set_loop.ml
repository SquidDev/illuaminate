open IlluaminateCore.Syntax
open IlluaminateCore
open! Linter

open struct
  module R = IlluaminateSemantics.Resolve
end

let linter =
  let tag = Error.make_tag Error.Warning "var:set-loop" in
  let stmt () (context : context) =
    let check (Var name as var) =
      let resolve = Data.get context.program R.key context.data in
      match R.get_definition var resolve with
      | { kind = Loop _; _ } ->
          [ note ~tag ~span:(Node.span name)
              (Printf.sprintf "Mutating loop variable %S." (Node.contents.get name))
          ]
      | _ -> []
    in
    function
    | AssignFunction { assignf_name = FVar var; _ } -> check var
    | Assign { assign_vars = vars; _ } ->
        SepList1.fold_left
          (fun x n ->
            match n with
            | NVar v -> check v @ x
            | NDot _ | NLookup _ -> x)
          [] vars
    | _ -> []
  in
  make_no_opt ~tags:[ tag ] ~stmt ()
