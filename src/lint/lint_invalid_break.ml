open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make Error.Error "syntax:invalid-break"

let msg = [ note ~tag "Using `break` outside a loop" ]

let rec has_loop = function
  | [] -> false
  | Stmt (Repeat _ | While _ | ForNum _ | ForIn _) :: _ -> true
  | Stmt (AssignFunction _ | LocalFunction _) :: _ -> false
  | Stmt (Do _ | If _) :: xs -> has_loop xs
  | Block _ :: xs -> has_loop xs
  | Expr (Fun _) :: _ -> false
  | (Bind | Name _ | FunctionName _ | Expr _ | Stmt _) :: _ -> failwith "Impossible break"

let stmt () (context : context) = function
  | Break _ when not (has_loop context.path) -> msg
  | _ -> []

let linter = make_no_opt ~tags:[ tag ] ~stmt ()
