open IlluaminateCore.Syntax
open IlluaminateCore
open! Linter

open struct
  module R = IlluaminateSemantics.Resolve
end

let linter =
  let open struct
    type options = { allow_toplevel_global : bool }
  end in
  let options =
    let open IlluaminateConfig in
    let open Term in
    let term =
      let+ allow_toplevel_global =
        field ~name:"allow-toplevel-global"
          ~comment:"Allow setting globals on the top-level of the module." ~default:false
          Converter.bool
      in
      { allow_toplevel_global }
    in
    Category.add term category
  in
  let tag = Error.Tag.make Error.Warning "var:set-global" in
  let rec is_toplevel = function
    | [] | [ Block _ ] -> true
    | Block _ :: xs -> is_toplevel xs
    | Stmt (AssignFunction _ | LocalFunction _) :: _ -> false
    | Stmt _ :: xs -> is_toplevel xs
    | (FunctionName _ | Name _ | Bind | Expr _) :: _ -> false
  in
  let unallowed { allow_toplevel_global } path =
    if allow_toplevel_global then not (is_toplevel path) else true
  in
  let stmt opts (context : context) = function
    | AssignFunction { assignf_name = FVar (Var name as var); _ } when unallowed opts context.path
      -> (
        let resolve = Data.get context.program R.key context.data in
        match R.get_definition var resolve with
        | { kind = Global; _ } ->
            [ note ~tag "Setting unknown global function %S." (Node.contents.get name) ]
        | _ -> [] )
    | _ -> []
  and name opts context name =
    match (context.path, name) with
    | Bind :: Stmt (Assign _) :: path, NVar (Var name as var) when unallowed opts path -> (
        let resolve = Data.get context.program R.key context.data in
        match R.get_definition var resolve with
        | { kind = Global; _ } ->
            [ note ~tag "Setting unknown global variable %S." (Node.contents.get name) ]
        | _ -> [] )
    | _ -> []
  in
  make ~options ~tags:[ tag ] ~stmt ~name ()
