open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let linter =
  (* TODO: Could totally have fixers for this. *)
  let tag = Error.make_tag Error.Warning "var:pointless-discard" in
  let note n = [ note ~span:(Node.span n) ~tag "Pointless discard variable `_`." ] in
  let check_args { args_args = args; _ } =
    match SepList0.last args with
    | Some (NamedArg (Var c)) when Node.contents.get c = "_" -> note c
    | _ -> []
  in
  let stmt () _ = function
    | Assign { assign_vars = vs; _ } -> (
      match SepList1.last.get vs with
      | NVar (Var c) when Node.contents.get c = "_" -> note c
      | _ -> [] )
    | Local { local_vars = vs; _ } | ForIn { forp_vars = SepList1.Cons1 _ as vs; _ } -> (
      (* for loops need at least one variable. *)
      match SepList1.last.get vs with
      | Var c when Node.contents.get c = "_" -> note c
      | _ -> [] )
    | LocalFunction { localf_args = args; _ } | AssignFunction { assignf_args = args; _ } ->
        check_args args
    | _ -> []
  and expr () _ = function
    | Fun { fun_args = args; _ } -> check_args args
    | _ -> []
  in
  make_no_opt ~tags:[ tag ] ~stmt ~expr ()
