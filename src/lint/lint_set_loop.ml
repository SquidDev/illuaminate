open IlluaminateCore.Syntax
open IlluaminateCore
open IlluaminateSemantics
open! Linter
module R = Resolve

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "var:set-loop"

let stmt () (context : context) r =
  let check (Var name as var) =
    let resolve = IlluaminateData.need context.data R.key context.file |> Option.get in
    match R.get_definition var resolve with
    | { kind = Loop _; _ } ->
        r.r ~tag ~span:(Node.span name) "Mutating loop variable %S." (Node.contents.get name)
    | _ -> ()
  in
  function
  | AssignFunction { assignf_name = FVar var; _ } -> check var
  | Assign { assign_vars = vars; _ } ->
      SepList1.iter
        (fun n ->
          match n with
          | NVar v -> check v
          | NDot _ | NLookup _ -> ())
        vars
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~stmt ()
