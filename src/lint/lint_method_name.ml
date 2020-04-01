open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make ~attr:[ Default ] ~level:Error "syntax:method-name"

let msg n = [ note ~tag ~span:(Node.span n) "Method names cannot appear at this position." ]

let stmt () _ = function
  | AssignFunction { assignf_name = FDot { tbl; _ } | FSelf { tbl; _ }; _ } ->
      let rec check = function
        | FSelf { meth; _ } -> msg meth
        | FDot { tbl; _ } -> check tbl
        | FVar _ -> []
      in
      check tbl
  | _ -> []

let linter = make_no_opt ~tags:[ tag ] ~stmt ()
