open IlluaminateCore.Syntax
open IlluaminateCore
open IlluaminateSemantics
open! Linter
module R = Resolve

let tag_generic = Error.Tag.make ~attr:[ Default; Unused ] ~level:Warning "var:unused"
let tag_arg = Error.Tag.make ~attr:[ Default; Unused ] ~level:Warning "var:unused-arg"
let tag_global = Error.Tag.make ~attr:[ Default; Unused ] ~level:Warning "var:unused-global"
let fix_var = Fixer.fix @@ fun (Var name) -> Ok (Var (Node.with_contents "_" name))

let fix_args ({ args_args = args; _ } as rest) =
  let rec go =
    let open SepList1 in
    function
    | Mono (DotArg _) -> None
    | Mono _ as x -> Some x
    | Cons1 (x, t, xs) ->
        Some
          (match go xs with
          | None -> Mono x
          | Some xs -> Cons1 (x, t, xs))
  in
  { rest with args_args = Option.bind args go }

let fix_expr =
  Fixer.fix @@ function
  | Fun ({ fun_args = args; _ } as rest) -> Ok (Fun { rest with fun_args = fix_args args })
  | _ -> Error "Expected function"

let fix_stmt =
  Fixer.fix @@ function
  | LocalFunction ({ localf_args = args; _ } as rest) ->
      Ok (LocalFunction { rest with localf_args = fix_args args })
  | AssignFunction ({ assignf_args = args; _ } as rest) ->
      Ok (AssignFunction { rest with assignf_args = fix_args args })
  | _ -> Error "Expected function"

let check_args (context : context) r fix { args_args; _ } =
  match SepList0.last args_args with
  | Some (DotArg dot) -> (
      let resolve = IlluaminateData.need context.data R.key context.file |> Option.get in
      (* If the list argument is a dot, and both it and the implicit {!arg} is unused, then warn. *)
      match R.get_dots_definition dot resolve with
      | { R.dot_usages = []; dot_implicit = Some { usages = []; _ }; _ } ->
          r.r ~tag:tag_arg ~fix ~span:(Node.span dot) "Unused varargs."
      | _ -> ())
  | _ -> ()

let var () context r (Var name as var) =
  let name = Node.contents.get name in
  if name <> "_" then
    match context.path with
    | Bind :: _ | Name (NVar _) :: Bind :: _ | FunctionName (FVar _) :: Bind :: _ -> (
        let resolve = IlluaminateData.need context.data R.key context.file |> Option.get in
        match R.get_definition var resolve with
        | { R.usages = []; kind; _ } ->
            let tag =
              match kind with
              | Global -> tag_global
              | Arg _ -> tag_arg
              | _ -> tag_generic
            in
            r.r ~fix:fix_var ~tag "Unused variable %S." name
        | _ -> ())
    | _ -> ()

let stmt () context r = function
  (* TODO: Ideally we'd have fixers for unused assignments which remove them. For now, we have to
     rely on the fact that pointless_discard will run afterwards. *)
  | LocalFunction { localf_args = args; _ } | AssignFunction { assignf_args = args; _ } ->
      check_args context r fix_stmt args
  | _ -> ()

let expr () context r = function
  | Fun { fun_args = args; _ } -> check_args context r fix_expr args
  | _ -> ()

let linter = make_no_opt ~tags:[ tag_generic; tag_arg; tag_global ] ~var ~expr ~stmt ()
