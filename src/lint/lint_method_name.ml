open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make ~attr:[ Default ] ~level:Error "syntax:method-name"

let stmt () _ r = function
  | AssignFunction { assignf_name = FDot { tbl; _ } | FSelf { tbl; _ }; _ } ->
      let rec check = function
        | FSelf { meth; _ } ->
            r.r ~tag ~span:(Node.span meth) "Method names cannot appear at this position."
        | FDot { tbl; _ } -> check tbl
        | FVar _ -> ()
      in
      check tbl
  | _ -> ()

let linter = make_no_opt ~tags:[ tag ] ~stmt ()
