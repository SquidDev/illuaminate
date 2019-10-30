open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let linter =
  let tag = Error.make_tag Error.Error "syntax:method-name" in
  let msg n = [ note ~tag ~span:(Node.span n) "Method names cannot appear at this position." ] in
  let stmt () _ = function
    | AssignFunction { assignf_name = FDot { tbl; _ } | FSelf { tbl; _ }; _ } ->
        let rec check = function
          | FSelf { meth; _ } -> msg meth
          | FDot { tbl; _ } -> check tbl
          | FVar _ -> []
        in
        check tbl
    | _ -> []
  in
  make_no_opt ~tags:[ tag ] ~stmt ()
