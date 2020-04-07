open IlluaminateCore.Syntax
open IlluaminateCore
open Linter

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

let check (t : BracketSpace.t) ~r ~before ~left ~right ~after =
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
     r.r
       ~span:(Span.of_span2 (Node.span before) (Node.span after))
       ~detail:(fun f -> Format.pp_print_string f m)
       ~tag "Inconsistent spacing"

let check_e t ~within = check t ~left:(First.expr.get within) ~right:(Last.expr.get within)

let check_args t ~r = function
  | { args_args = None; _ } -> ()
  | { args_open; args_close; args_args = Some args } ->
      check t ~r ~before:args_open ~after:args_close
        ~left:(SepList1.first.get args |> First.arg.get)
        ~right:(SepList1.last.get args |> Last.arg.get)

let check_call t ~r call =
  let (Call { args; _ } | Invoke { args; _ }) = call in
  match args with
  | CallArgs { args = None; _ } | CallTable _ | CallString _ -> ()
  | CallArgs { open_a; args = Some args; close_a } ->
      check t ~r ~before:open_a ~after:close_a
        ~left:(SepList1.first.get args |> First.expr.get)
        ~right:(SepList1.last.get args |> Last.expr.get)

let expr (t : Opt.t) _ r = function
  | Table { table_open; table_close; table_body = (e, _) :: _ as es } ->
      let right =
        match CCList.last_opt es |> Option.get with
        | _, Some s -> s
        | e, None -> Last.table_item.get e
      in
      check ~r t.table ~before:table_open ~left:(First.table_item.get e) ~right ~after:table_close;

      let go_item = function
        | ExprPair e, _ -> check_e t.index ~r ~before:e.open_k ~within:e.key ~after:e.close_k
        | Array _, _ | RawPair _, _ -> ()
      in
      List.iter go_item es
  | ECall c -> check_call t.call ~r c
  | Parens e -> check_e t.parens ~r ~before:e.paren_open ~within:e.paren_expr ~after:e.paren_close
  | Fun e -> check_args t.args ~r e.fun_args
  | _ -> ()

let name (t : Opt.t) _ r = function
  | NLookup e -> check_e t.index ~r ~before:e.open_k ~within:e.key ~after:e.close_k
  | NVar _ | NDot _ -> ()

let stmt (t : Opt.t) _ r = function
  | LocalFunction { localf_args = args; _ } | AssignFunction { assignf_args = args; _ } ->
      check_args t.args ~r args
  | SCall c -> check_call t.call ~r c
  | _ -> ()

let linter = make ~options:Opt.options ~expr ~stmt ~name ~tags:[ tag ] ()
