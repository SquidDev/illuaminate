open IlluaminateCore.Syntax
open IlluaminateCore
open Linter
open Lens

let linter =
  let open struct
    type options = { clarifying : bool }
  end in
  let options =
    let open IlluaminateConfig in
    let open Term in
    let term =
      let+ clarifying =
        field ~name:"allow-clarifying-parens"
          ~comment:"Allow parenthesis which clarify syntactic ambiguities." ~default:false
          Converter.bool
      in
      { clarifying }
    in
    Category.add term category
  in
  let tag = Error.Tag.make Error.Note "syntax:redundant-parens" in
  let unpack = function
    | Node.SimpleNode _ -> []
    | Node.Node { leading_trivia = l; trailing_trivia = t; _ } -> Node.join_trivia l t
  in
  let rec unwrap = function
    | Parens { paren_open = o; paren_expr = e; paren_close = c } ->
        let l, e, t = unwrap e in
        let l' = unpack o and t' = unpack c in
        (Node.join_trivia l' l, e, Node.join_trivia t t')
    | e -> ([], e, [])
  in
  let unwrap_all =
    (* The fixer attempts to append the leading/trailing trivia to the current expression. *)
    let fix e =
      match e with
      | Parens _ ->
          let open Lens in
          let l, e, t = unwrap e in
          let e =
            match l with
            | [] -> e
            | _ -> ((First.expr -| Node.leading_trivia) %= fun x -> Node.join_trivia x l) @@ e
          in
          let e =
            match t with
            | [] -> e
            | _ -> ((Last.expr -| Node.trailing_trivia) %= fun x -> Node.join_trivia t x) @@ e
          in
          Ok e
      | _ -> Error "Expected Parens"
    in
    [ note ~tag ~fix:(FixOne fix) "Unnecessary parenthesis." ]
  in
  let unwrap_most =
    (* The fixer attempts to append the leading/trailing trivia to the outer parenthesis. *)
    let fix = function
      | Parens { paren_open; paren_expr = e; paren_close } ->
          let open Lens in
          let l, e, t = unwrap e in
          Parens
            { paren_open = (Node.trailing_trivia %= fun x -> Node.join_trivia x l) @@ paren_open;
              paren_expr = e;
              paren_close = (Node.leading_trivia %= fun x -> Node.join_trivia t x) @@ paren_close
            }
          |> Result.ok
      | _ -> Error "Expected Parens"
    in
    [ note ~tag ~fix:(FixOne fix) "Unnecessary parenthesis." ]
  in
  let contains p = function
    | None -> false
    | Some q -> p == q
  in
  let expr { clarifying } { path; _ } parens =
    (* Determine whether this term is in a call/index context. *)
    let is_fn_or_tbl () =
      match path with
      | Expr (ECall (Call { fn = x; _ } | Invoke { obj = x; _ })) :: _
      | Stmt (SCall (Call { fn = x; _ } | Invoke { obj = x; _ })) :: _ ->
          x == parens
      | Name (NDot { tbl; _ } | NLookup { tbl; _ }) :: _ -> tbl == parens
      | _ -> false
    in
    (* Determine if this term is in a variadic context. *)
    let is_variadic () =
      match path with
      (* If we're the last term of a call or return statement. *)
      | Expr
          (ECall
            (Call { args = CallArgs { args; _ }; _ } | Invoke { args = CallArgs { args; _ }; _ }))
        :: _
      | Stmt
          (SCall
            (Call { args = CallArgs { args; _ }; _ } | Invoke { args = CallArgs { args; _ }; _ }))
        :: _
      | Stmt (Return { return_vals = args; _ }) :: _ ->
          SepList0.last args |> contains parens
      (* If we're the last term in a table. *)
      | Expr (Table { table_body; _ }) :: _ ->
          let rec go = function
            | [] -> false
            | [ (Array x, _) ] -> x == parens
            | _ :: xs -> go xs
          in
          go table_body
      | Stmt (Local { local_vars = vs; local_vals = Some (_, es); _ }) :: _ ->
          SepList1.length vs > SepList1.length es && SepList1.last.get es == parens
      | Stmt (Assign { assign_vars = vs; assign_vals = es; _ }) :: _ ->
          SepList1.length vs > SepList1.length es && SepList1.last.get es == parens
      | _ -> false
    in
    (* Determine if this term is in a context of at least the given precedence. *)
    let has_precedence current =
      let prec =
        match path with
        | Expr (UnOp { unop_op; _ }) :: _ -> Some (UnOp.precedence (unop_op ^. Node.contents))
        | Expr (BinOp { binop_lhs; binop_op; binop_rhs }) :: _ ->
            let op = binop_op ^. Node.contents in
            if binop_lhs == parens then Some (BinOp.left_precedence op)
            else if binop_rhs == parens then Some (BinOp.right_precedence op)
            else None
        | _ -> None
      in
      match prec with
      | None -> false
      | Some pre -> pre <= current || clarifying
    in
    match (parens, path) with
    (* Skip parens nested inside others - we'll always pick up the outer one. *)
    | _, Expr (Parens _) :: _ -> []
    | Parens { paren_expr = expr; _ }, _ ->
        let rec go msg = function
          (* Variables _never_ need to be wrapped in parens. *)
          | Ref _ -> unwrap_all
          (* If we've some latent parens, then we warn. However, we may need to leave some parens if
             required. *)
          | Parens { paren_expr = e; _ } -> go unwrap_most e
          (* If we've got raw literals, then they only need to be wrapped when calling/indexing
             them. *)
          | Nil _ | True _ | False _ | Number _ | String _ | Int _ | MalformedNumber _ | Fun _
          | Table _ ->
              if is_fn_or_tbl () then msg else unwrap_all
          (* If we're a term which yields a variable number of arguments, we should protect when the
             last argument to a call, or last value in a binding. *)
          | ECall _ -> if is_variadic () then msg else unwrap_all
          | Dots _ -> if is_fn_or_tbl () || is_variadic () then msg else unwrap_all
          (* Operators - check indexing, and then precedence. *)
          | UnOp { unop_op = op; _ } ->
              if is_fn_or_tbl () || has_precedence (op ^. Node.contents |> UnOp.precedence) then msg
              else unwrap_all
          | BinOp { binop_op = op; _ } ->
              if is_fn_or_tbl () || has_precedence (op ^. Node.contents |> BinOp.precedence) then
                msg
              else unwrap_all
        in
        go [] expr
    | _ -> []
  in
  make ~options ~tags:[ tag ] ~expr ()
