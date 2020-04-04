open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

let tag = Error.Tag.make ~attr:[ Default; Unused ] ~level:Warning "var:pointless-discard"

(** A variable is a "discard variable" if it is named exactly "_". *)
let is_discard (Var n) = Node.contents.get n = "_"

module Fix = struct
  type 'a stripped_list =
    | Present of 'a SepList1.t
    | Empty of Node.trivial Span.spanned list

  let to_option = function
    | Present x -> Some x
    | Empty _ -> None

  (** Remove any trailing discard variables from a list of args, vars or similar. *)
  let rec fix_list1 is_discard trailing : 'a SepList1.t -> ('a stripped_list, string) result =
    function
    | SepList1.Mono v ->
        if is_discard v then Ok (Empty (trailing.Lens.get v))
        else Error "Not terminated by a discarded variable."
    | Cons1 (v, s, xs) -> (
      match fix_list1 is_discard trailing xs with
      | Ok (Empty tt) ->
          Ok
            ( if is_discard v then Empty tt
            else Present (SepList1.Mono (trailing.Lens.over (fun t -> t @ tt) v)) )
      | Ok (Present xs) -> Ok (Present (SepList1.Cons1 (v, s, xs)))
      | Error e -> Error e )

  (** Fix a list of arguments. *)
  let fix_args =
    let check_arg = function
      | NamedArg x -> is_discard x
      | DotArg _ -> false
    in
    function
    | { args_args = None; _ } -> Error "No arguments in this function"
    | { args_args = Some x; _ } as a ->
        fix_list1 check_arg Lens.(Syntax.Last.arg -| Node.trailing_trivia) x
        |> Result.map (fun x -> { a with args_args = to_option x })

  (** Attempt to flatten an expression into a series of statements. *)
  let rec flatten : expr -> (stmt list, string) result = function
    | Ref (NVar _)
    | Dots _ | Nil _ | True _ | False _ | Number _ | MalformedNumber _ | Int _ | String _ | Fun _ ->
        Ok []
    | Parens { paren_expr = x; _ } -> flatten x
    | Table { table_body; _ } ->
        let rec items xs =
          let ( let+ ) = Result.bind in
          function
          | [] -> Ok (List.rev xs)
          | ((Array e | RawPair { value = e; _ }), _) :: ts ->
              let+ e = flatten e in
              items (e @ xs) ts
          | (ExprPair { key; value; _ }, _) :: ts ->
              let+ k = flatten key in
              let+ v = flatten value in
              items (v @ k @ xs) ts
        in
        items [] table_body
    | ECall c -> Ok [ SCall c ]
    | (UnOp _ | BinOp _ | Ref _) as e ->
        if IlluaminateSemantics.Pure.Safe.expr e then Ok [] else Error "Impure term"

  (** Attempt to flatten multiple expressions into a series of statements. *)
  let rec flatten1 : expr SepList1.t -> (stmt list, string) result = function
    | Mono v -> flatten v
    | Cons1 (v, _, vs) -> (
      match flatten v with
      | Error e -> Error e
      | Ok v -> flatten1 vs |> Result.map (fun vs -> v @ vs) )

  (** Checks arbitrary statements, removing redundant trailing discards. If the entire statement is
      redundant, we attempt to flatten the statements. *)
  let fix_assignment =
    let is_discard_name = function
      | NVar v -> is_discard v
      | _ -> false
    in
    Fixer.block @@ function
    | Assign ({ assign_vars = vs; assign_vals = es; _ } as st) -> (
      match fix_list1 is_discard_name Lens.(Syntax.Last.name -| Node.trailing_trivia) vs with
      | Error e -> Error e
      | Ok (Present vs) -> Ok [ Assign { st with assign_vars = vs } ]
      | Ok (Empty _) -> flatten1 es )
    | Local ({ local_vars = vs; local_vals = es; _ } as st) -> (
      match fix_list1 is_discard Lens.(Syntax.Last.var -| Node.trailing_trivia) vs with
      | Error e -> Error e
      | Ok (Present vs) -> Ok [ Local { st with local_vars = vs } ]
      | Ok (Empty _) -> (
        match es with
        | None -> Ok []
        | Some (_, vs) -> flatten1 vs ) )
    | ForIn ({ forp_vars = SepList1.Cons1 (v, s, vs); _ } as forp) -> (
        let lens = Lens.(Syntax.Last.var -| Node.trailing_trivia) in
        match fix_list1 is_discard lens vs with
        | Error e -> Error e
        | Ok (Empty tt) ->
            Ok [ ForIn { forp with forp_vars = Mono (lens.over (fun t -> t @ tt) v) } ]
        | Ok (Present vs) -> Ok [ ForIn { forp with forp_vars = Cons1 (v, s, vs) } ] )
    | LocalFunction _ | AssignFunction _ -> Ok []
    | _ -> Error "Oh no"

  (** Checks the arguments of a function. *)
  let fix_expr_args =
    Fixer.fix @@ function
    | Fun ({ fun_args = args; _ } as fn) ->
        fix_args args |> Result.map (fun x -> Fun { fn with fun_args = x })
    | _ -> Error "Not a function."

  (** Checks the arguments of a statement. *)
  let fix_stmt_args =
    Fixer.fix @@ function
    | LocalFunction ({ localf_args = args; _ } as fn) ->
        fix_args args |> Result.map (fun x -> LocalFunction { fn with localf_args = x })
    | AssignFunction ({ assignf_args = args; _ } as fn) ->
        fix_args args |> Result.map (fun x -> AssignFunction { fn with assignf_args = x })
    | _ -> Error "Not a function"
end

let note fix (Var n) = [ note ~fix ~span:(Node.span n) ~tag "Pointless discard variable `_`." ]

let check_args fixer { args_args = args; _ } =
  match SepList0.last args with
  | Some (NamedArg v) when is_discard v -> note fixer v
  | _ -> []

let stmt () _ = function
  | Assign { assign_vars = vs; _ } -> (
    (* TODO: Can we match patterns like a, _, b = 0, 1, 2, where it's safe to discard when it's in
       the middle? *)
    match SepList1.last.get vs with
    | NVar v when is_discard v -> note Fix.fix_assignment v
    | _ -> [] )
  | Local { local_vars = vs; _ } | ForIn { forp_vars = SepList1.Cons1 (_, _, vs); _ } ->
      (* For loops need at least one variable, hence the odd pattern. *)
      let v = SepList1.last.get vs in
      if is_discard v then note Fix.fix_assignment v else []
  | LocalFunction { localf_var = v; localf_args = args; _ } ->
      let main = check_args Fix.fix_stmt_args args in
      if is_discard v then note Fix.fix_assignment v @ main else main
  | AssignFunction { assignf_name = n; assignf_args = args; _ } -> (
      let main = check_args Fix.fix_stmt_args args in
      match n with
      | FVar v when is_discard v -> note Fix.fix_assignment v @ main
      | _ -> main )
  | _ -> []

let expr () _ = function
  | Fun { fun_args = args; _ } -> check_args Fix.fix_expr_args args
  | _ -> []

let linter = make_no_opt ~tags:[ tag ] ~stmt ~expr ()
