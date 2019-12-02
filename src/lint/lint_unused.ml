open IlluaminateCore.Syntax
open IlluaminateCore
open! Linter

open struct
  module R = IlluaminateSemantics.Resolve
end

let linter =
  let tag_generic = Error.Tag.make Error.Warning "var:unused" in
  let tag_arg = Error.Tag.make Error.Warning "var:unused-arg" in
  let tag_global = Error.Tag.make Error.Warning "var:unused-global" in
  let fix_var = FixOne (fun (Var name) -> Ok (Var (Node.with_contents "_" name))) in
  let fix_args ({ args_args = args; _ } as rest) =
    let rec go =
      let open SepList1 in
      function
      | Mono (DotArg _) -> None
      | Mono _ as x -> Some x
      | Cons1 (x, t, xs) ->
          Some
            ( match go xs with
            | None -> Mono x
            | Some xs -> Cons1 (x, t, xs) )
    in
    { rest with args_args = Option.bind args go }
  in
  let fix_expr =
    FixOne
      (function
      | Fun ({ fun_args = args; _ } as rest) -> Ok (Fun { rest with fun_args = fix_args args })
      | _ -> Error "Expected function")
  and fix_stmt =
    FixOne
      (function
      | LocalFunction ({ localf_args = args; _ } as rest) ->
          Ok (LocalFunction { rest with localf_args = fix_args args })
      | AssignFunction ({ assignf_args = args; _ } as rest) ->
          Ok (AssignFunction { rest with assignf_args = fix_args args })
      | _ -> Error "Expected function")
  in
  let check_args (context : context) fix { args_args; _ } =
    match SepList0.last args_args with
    | Some (DotArg dot) -> (
        let resolve = Data.get context.program R.key context.data in
        (* If the list argument is a dot, and both it and the implicit {!arg} is unused, then warn. *)
        match R.get_dots dot resolve with
        | { R.dot_usages = []; dot_implicit = Some { usages = []; _ }; _ } ->
            [ note ~tag:tag_generic ~fix ~span:(Node.span dot) "Unused varargs." ]
        | _ -> [] )
    | _ -> []
  in
  let var () context (Var name as var) =
    let name = Node.contents.get name in
    if name = "_" then []
    else
      match context.path with
      | Bind :: _ | Name (NVar _) :: Bind :: _ | FunctionName (FVar _) :: Bind :: _ -> (
          let resolve = Data.get context.program R.key context.data in
          match R.get_definition var resolve with
          | { R.usages = []; kind; _ } ->
              let tag =
                match kind with
                | Global -> tag_global
                | Arg _ -> tag_arg
                | _ -> tag_generic
              in
              [ note ~fix:fix_var ~tag (Printf.sprintf "Unused variable %S." name) ]
          | _ -> [] )
      | _ -> []
  and stmt () context = function
    | LocalFunction { localf_args = args; _ } | AssignFunction { assignf_args = args; _ } ->
        check_args context fix_stmt args
    | _ -> []
  and expr () context = function
    | Fun { fun_args = args; _ } -> check_args context fix_expr args
    | _ -> []
  in
  make_no_opt ~tags:[ tag_generic; tag_arg; tag_global ] ~var ~expr ~stmt ()
