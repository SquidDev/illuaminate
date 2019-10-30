open IlluaminateCore.Syntax
open IlluaminateCore
open! Linter

open struct
  module R = IlluaminateSemantics.Resolve
end

let linter =
  let tag = Error.make_tag Error.Warning "var:set-global" in
  let stmt () (context : context) = function
    | AssignFunction { assignf_name = FVar (Var name as var); _ } -> (
        let resolve = Data.get context.program R.key context.data in
        match R.get_definition var resolve with
        | { kind = Global; _ } ->
            [ note ~tag
                (Printf.sprintf "Setting unknown global function %S." (Node.contents.get name))
            ]
        | _ -> [] )
    | _ -> []
  and name () context name =
    match (context.path, name) with
    | Bind :: Stmt (Assign _) :: _, NVar (Var name as var) -> (
        let resolve = Data.get context.program R.key context.data in
        match R.get_definition var resolve with
        | { kind = Global; _ } ->
            [ note ~tag
                (Printf.sprintf "Setting unknown global variable %S." (Node.contents.get name))
            ]
        | _ -> [] )
    | _ -> []
  in
  make_no_opt ~tags:[ tag ] ~stmt ~name ()
