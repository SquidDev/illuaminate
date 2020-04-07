open IlluaminateCore.Syntax
open IlluaminateCore
open Linter
open Lens

(** How terms within brackets should be spaced. *)
module BracketSpace = struct
  type t =
    | Space  (** Have a space between the brackets and the contents. *)
    | NoSpace  (** Do not have a space between the brackets and their contents.*)
    | Consistent  (** No preference in spaces, but warn when they are inconsistent. *)
    | None  (** We do not care about spacing. *)

  let converter =
    IlluaminateConfig.Term.Converter.enum ~ty:"spacing"
      [ ("space", Space); ("no-space", NoSpace); ("consistent", Consistent); ("none", None) ]

  let field = IlluaminateConfig.Term.field ~default:Consistent converter
end

module ActualSpacing = struct
  type t =
    | NoSpace
    | Space
    | Newline

  let rec get t : Node.trivial Span.spanned list -> t = function
    | [] -> t
    | { value = LineComment _; _ } :: _ -> Newline
    | { value = BlockComment (_, s) | Whitespace s; _ } :: _ when String.contains s '\n' -> Newline
    | { value = BlockComment _; _ } :: xs -> get t xs
    | { value = Whitespace _; _ } :: xs -> get Space xs

  let get xs = function
    | Newline -> Newline
    | (Space | NoSpace) as t -> get t xs

  let between l r = NoSpace |> get (Node.trailing_trivia.get l) |> get (Node.leading_trivia.get r)

  let is_comment : Node.trivial Span.spanned -> bool = function
    | { value = Whitespace _; _ } -> false
    | { value = BlockComment _ | LineComment _; _ } -> true

  let adjust_trailing ~space ~current =
    Node.trailing_trivia.over @@ fun trivia ->
    match (space, current) with
    (* We insert whitespace on the leading position of the next token, hence need to do nothing when
       this is a space. *)
    | _, Newline | true, _ | false, NoSpace -> trivia
    | false, Space -> List.filter is_comment trivia

  let adjust_leading ~space ~current ~span =
    Node.leading_trivia.over @@ fun trivia ->
    match (space, current) with
    | _, Newline | true, Space | false, NoSpace -> trivia
    | true, NoSpace -> { span; value = Whitespace " " } :: trivia
    | false, Space -> List.filter is_comment trivia
end

module Opt = struct
  type t =
    { call : BracketSpace.t;  (** Call arguments. *)
      args : BracketSpace.t;  (** Function definitions. *)
      parens : BracketSpace.t;  (** Parenthesis expressions *)
      table : BracketSpace.t;  (** Table expressions. *)
      index : BracketSpace.t  (** Table indexes, within names and table expressions. *)
    }

  let options =
    let open IlluaminateConfig in
    let open Term in
    let term =
      group ~name:"bracket-spaces"
        ~comment:"Spaces within bracketed terms, such as tables or function calls."
      @@ let+ call = BracketSpace.field ~name:"call" ~comment:"Spaces within call arguments."
         and+ args =
           BracketSpace.field ~name:"function-args" ~comment:"Spaces within function arguments."
         and+ parens =
           BracketSpace.field ~name:"parens" ~comment:"Spaces within parenthesised expressions."
         and+ table = BracketSpace.field ~name:"table" ~comment:"Spaces within tables."
         and+ index = BracketSpace.field ~name:"index" ~comment:"Spaces within table indexes." in
         { call; args; parens; table; index }
    in

    Category.add term category
end

let tag = Error.Tag.make ~attr:[ Default ] ~level:Note "format:bracket-space"

(** Visitors which work on both fixers and checkers. See {!Fix.at} and {!Check.at}. *)
module Generic = struct
  let args t ~default ~go = function
    | { args_args = None; _ } -> default
    | { args_args = Some _; _ } as a ->
        go t ~before:Args.args_open ~after:Args.args_close
          ~left:(Args.args_args -| Lenses.unsafe_option -| SepList1.first -| First.arg)
          ~right:(Args.args_args -| Lenses.unsafe_option -| SepList1.last -| Last.arg)
          a

  let last_table_item =
    let get = function
      | _, Some s -> s
      | e, None -> Last.table_item.get e
    and over f = function
      | e, Some s -> (e, Some (f s))
      | e, None -> (Last.table_item.over f e, None)
    in
    { get; over }

  let tbl_left =
    Table.table_body -| Lenses.head -| Lenses.unsafe_option -| Lenses.fst -| First.table_item

  and tbl_right = Table.table_body -| Lenses.last -| Lenses.unsafe_option -| last_table_item

  let table t ~default ~go = function
    | { table_body = []; _ } -> default
    | { table_body = _ :: _; _ } as tbl ->
        go t ~before:Table.table_open ~after:Table.table_close ~left:tbl_left ~right:tbl_right tbl

  let parens t ~go x =
    go t ~before:Paren_expr.paren_open ~after:Paren_expr.paren_close
      ~left:(Paren_expr.paren_expr -| First.expr)
      ~right:(Paren_expr.paren_expr -| Last.expr)
      x
end

module Fix = struct
  let go (t : BracketSpace.t) ~before ~left ~right ~after =
    let l = ActualSpacing.between before left and r = ActualSpacing.between right after in
    let adjust ~space =
      ActualSpacing.
        ( adjust_trailing ~space ~current:l before,
          adjust_leading ~space ~current:l ~span:(Node.span left),
          adjust_trailing ~space ~current:r,
          adjust_leading ~space ~current:r ~span:(Node.span after) after )
      |> Result.ok
    in
    match (t, l, r) with
    | None, _, _ -> Error "Already correct"
    | _, Newline, _ | _, _, Newline -> Error "Already correct"
    | (Consistent | Space), Space, Space -> Error "Already correct"
    | (Consistent | NoSpace), NoSpace, NoSpace -> Error "Already correct"
    | (Space | Consistent), _, _ ->
        (* For now, we just assume a "consistent" style should add spaces. *)
        adjust ~space:true
    | NoSpace, _, _ -> adjust ~space:false

  let at t ~before ~after ~left ~right x =
    go t ~before:(x ^. before) ~after:(x ^. after) ~left:(x ^. left) ~right:(x ^. right)
    |> Result.map @@ fun (b, l, r, a) -> x |> before ^= b |> left %= l |> right %= r |> after ^= a

  let within t ~within ~before ~after =
    go t ~left:(within ^. First.expr) ~right:(within ^. Last.expr) ~before ~after
    |> Result.map @@ fun (b, l, r, a) -> (b, within |> First.expr %= l |> Last.expr %= r, a)

  let call_args t =
    Fixer.fix @@ function
    | CallArgs { args = None; _ } | CallTable _ | CallString _ -> Error "Malformed args"
    | CallArgs { open_a; args = Some args; close_a } ->
        go t ~before:open_a ~after:close_a
          ~left:(SepList1.first.get args |> First.expr.get)
          ~right:(SepList1.last.get args |> Last.expr.get)
        |> Result.map @@ fun (open_a, l, r, close_a) ->
           CallArgs
             { open_a;
               close_a;
               args =
                 args
                 |> (SepList1.first -| First.expr) %= l
                 |> (SepList1.last -| Last.expr) %= r
                 |> Option.some
             }

  let table_item t =
    Fixer.fix @@ function
    | ExprPair e ->
        within t ~before:e.open_k ~within:e.key ~after:e.close_k
        |> Result.map @@ fun (b, w, a) -> ExprPair { e with open_k = b; close_k = a; key = w }
    | Array _ | RawPair _ -> Error "Not a pair"

  let name t =
    Fixer.fix @@ function
    | NLookup e ->
        within t ~before:e.open_k ~within:e.key ~after:e.close_k
        |> Result.map @@ fun (b, w, a) -> NLookup { e with open_k = b; key = w; close_k = a }
    | NVar _ | NDot _ -> Error "Not a lookup name"
end

module Check = struct
  let go (t : BracketSpace.t) ~r ~source ~kind ~fix ~before ~left ~right ~after =
    let message =
      match (t, ActualSpacing.between before left, ActualSpacing.between right after) with
      | None, _, _ -> None
      | _, Newline, _ | _, _, Newline -> None
      | (Consistent | Space), Space, Space -> None
      | (Consistent | NoSpace), NoSpace, NoSpace -> None
      | Space, NoSpace, _ | Space, _, NoSpace -> Some "Should have spaces within the brackets."
      | NoSpace, _, Space | NoSpace, Space, _ -> Some "Should not have spaces within the brackets."
      | Consistent, NoSpace, Space ->
          Some "No space after the opening bracket, but spaces before the closing one."
      | Consistent, Space, NoSpace ->
          Some "Spaces after the opening bracket, but no spaces before the closing one."
    in

    message
    |> Option.iter @@ fun m ->
       r.e
         ~span:(Span.of_span2 (Node.span before) (Node.span after))
         ~detail:(fun f -> Format.pp_print_string f m)
         ~source ~kind ~fix:(fix t) ~tag "Inconsistent spacing"

  let at t ~r ~source ~kind ~fix ~before ~after ~left ~right x =
    go t ~r ~source ~kind ~fix ~before:(x ^. before) ~after:(x ^. after) ~left:(x ^. left)
      ~right:(x ^. right)

  let expr t ~within = go t ~left:(First.expr.get within) ~right:(Last.expr.get within)

  let call t ~r call =
    let (Call { args; _ } | Invoke { args; _ }) = call in
    match args with
    | CallArgs { args = None; _ } | CallTable _ | CallString _ -> ()
    | CallArgs { open_a; args = Some a; close_a } ->
        go t ~r ~source:args ~kind:CallArgs ~fix:Fix.call_args ~before:open_a ~after:close_a
          ~left:(SepList1.first.get a |> First.expr.get)
          ~right:(SepList1.last.get a |> Last.expr.get)

  let on l f x = x ^. l |> f |> Result.map (fun y -> (l ^= y) x)

  let apply ~r ~source ~kind ~check ~lens ~fix x =
    check ~go:(at ~r ~source ~kind ~fix:(fun t -> Fixer.fix @@ on lens (fix t ~go:Fix.at))) x

  let args t ~r source =
    apply ~r ~source ~kind:Args ~check:(Generic.args t ~default:()) ~lens:Lenses.id
      ~fix:(Generic.args ~default:(Error "Empty args"))
      source
end

let expr (t : Opt.t) _ r = function
  | Table tbl as source ->
      Check.apply ~r ~source ~kind:Expr ~lens:Expr._Table
        ~check:(Generic.table t.table ~default:())
        ~fix:(Generic.table ~default:(Error "Empty table"))
        tbl;

      let go_item = function
        | (ExprPair e as source), _ ->
            Check.expr t.index ~r ~kind:TableItem ~source ~fix:Fix.table_item ~before:e.open_k
              ~within:e.key ~after:e.close_k
        | Array _, _ | RawPair _, _ -> ()
      in
      List.iter go_item tbl.table_body
  | ECall c -> Check.call t.call ~r c
  | Parens e as source ->
      Check.apply ~r ~source ~kind:Expr ~lens:Expr._Parens ~check:(Generic.parens t.parens)
        ~fix:Generic.parens e
  | Fun e -> Check.args t.args ~r e.fun_args
  | _ -> ()

let name (t : Opt.t) _ r = function
  | NLookup e as source ->
      Check.expr t.index ~r ~kind:Name ~source ~fix:Fix.name ~before:e.open_k ~within:e.key
        ~after:e.close_k
  | NVar _ | NDot _ -> ()

let stmt (t : Opt.t) _ r = function
  | LocalFunction { localf_args = args; _ } | AssignFunction { assignf_args = args; _ } ->
      Check.args t.args ~r args
  | SCall c -> Check.call t.call ~r c
  | _ -> ()

let linter = make ~options:Opt.options ~expr ~stmt ~name ~tags:[ tag ] ()
