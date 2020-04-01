open IlluaminateCore.Syntax
open IlluaminateCore
open IlluaminateSemantics
open! Linter
module R = Resolve

module Opt = struct
  type t = { allow_toplevel_global : bool }

  let parser =
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
end

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "var:set-global"

let rec is_toplevel = function
  | [] | [ Block _ ] -> true
  | Block _ :: xs -> is_toplevel xs
  | Stmt (AssignFunction _ | LocalFunction _) :: _ -> false
  | Stmt _ :: xs -> is_toplevel xs
  | (FunctionName _ | Name _ | Bind | Expr _) :: _ -> false

let unallowed { Opt.allow_toplevel_global } path =
  if allow_toplevel_global then not (is_toplevel path) else true

let stmt opts (context : context) = function
  | AssignFunction { assignf_name = FVar (Var name as var); _ } when unallowed opts context.path
    -> (
      let resolve = IlluaminateData.need context.data R.key context.program in
      match R.get_definition var resolve with
      | { kind = Global; _ } ->
          [ note ~span:(Spanned.var var) ~tag "Setting unknown global function %S."
              (Node.contents.get name)
          ]
      | _ -> [] )
  | _ -> []

let name opts context name =
  match (context.path, name) with
  | Bind :: Stmt (Assign _) :: path, NVar (Var name as var) when unallowed opts path -> (
      let resolve = IlluaminateData.need context.data R.key context.program in
      match R.get_definition var resolve with
      | { kind = Global; _ } ->
          [ note ~tag "Setting unknown global variable %S." (Node.contents.get name) ]
      | _ -> [] )
  | _ -> []

let linter = make ~options:Opt.parser ~tags:[ tag ] ~stmt ~name ()
