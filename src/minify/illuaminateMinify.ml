open IlluaminateCore
open Syntax
open Lens
module R = IlluaminateSemantics.Resolve

module Emitter = struct
  type t = Format.formatter

  let trivial out = function
    | Node.Whitespace " " -> Format.pp_print_space out ()
    | x -> Emit.trivial out x

  let trivial_span out { Span.value; _ } = trivial out value

  let twith kind f out x =
    Format.pp_open_stag out (Emit.Token kind);
    f out x;
    Format.pp_close_stag out ()

  let node ~kind body out = function
    | Node.SimpleNode { contents } -> twith kind body out contents
    | Node.Node { leading_trivia; contents; trailing_trivia; _ } ->
        List.iter (trivial_span out) leading_trivia;
        twith kind body out contents;
        List.iter (trivial_span out) trailing_trivia
end

module Emit = struct
  include Emit.Make (Emitter)

  let flush out = Format.pp_print_flush out ()

  let with_wrapping out fmt = Format.pp_set_margin out 80; Format.kfprintf flush out fmt
end

type token_kind =
  | OBracket
  | LongString
  | Ident of char
  | Number
  | Symbol
  | Concat
  | Minus

let needs_space (l : token_kind) (r : token_kind) =
  match (l, r) with
  (* 'a[ [[' is not valid. 'a[ [' should never happen, but just in case. *)
  | OBracket, (OBracket | LongString) -> true
  | OBracket, _ -> false
  (* These should be safe anywhere. *)
  | (LongString | Symbol), _ -> false
  (* 'not a' clearly needs spaces. Arguably 'a .2' does, but thankfully we'll never parse that .*)
  | Ident _, Ident _ -> true
  | Ident _, _ -> false
  (* We cannot compress '2 else' or '2 ..' as they're then consumed as part of the number. *)
  | Number, (Ident ('e' | 'E' | 'p' | 'P' | 'x' | 'X') | Concat) -> true
  | Number, _ -> false
  (* ' - -' becomes a comment. *)
  | Minus, Minus -> true
  | Minus, _ -> false
  (* _Technically this is safe, but '.. .2' does not parse, so we don't accept it. *)
  | Concat, Number -> true
  | Concat, _ -> false

let classify_string x : token_kind =
  match x.[0] with
  | '\'' | '"' -> Symbol
  | '[' -> LongString
  | _ -> failwith "Impossible string"

let token_ident : Token.t -> token_kind = function
  | Int _ | MalformedNumber _ | Number _ -> Number
  | String (_, x) -> classify_string x
  | Concat -> Concat
  | OSquare -> OBracket
  | Sub -> Minus
  | ( And | Break | Do | Else | ElseIf | End | False | For | Function | Ident _ | If | In | Local
    | Nil | Not | Or | Repeat | Return | Then | True | Until | While ) as t ->
      Ident (Token.show t).[0]
  | Add | CBrace | Colon | Comma | CParen | CSquare | Div | Dot | Dots | EoF | Eq | Equals | Ge | Gt
  | Le | Len | Lt | Mod | Mul | Ne | OBrace | OParen | Pow | Semicolon ->
      Symbol

(** Removes trivia from a node. We keep track of what sort of token the previous one was, and
    whether it needs a space around it. We use this to ensure that [local x] is not printed as
    [localx], but still allow things like [f()].*)
class remove_trivia =
  object (self)
    inherit Syntax.map

    val mutable last_kind = Symbol

    method private handle_node : 'a. now:token_kind -> ('a -> 'a) -> 'a Node.t -> 'a Node.t =
      fun ~now f node ->
        let open Node in
        let node =
          match node with
          | SimpleNode { contents } -> SimpleNode { contents = f contents }
          | Node { contents; span; _ } ->
              let leading =
                if needs_space last_kind now then [ { Span.span; value = Whitespace " " } ] else []
              in
              Node { contents; span; leading_trivia = leading; trailing_trivia = [] }
        in
        last_kind <- now;
        node

    method! node f x =
      (* The only raw nodes we have in the tree are where identifiers are expected /after/ some
         other symbol. Thus we never need worry about being immediately preceded with an number.
         Yes, this is ugly. *)
      self#handle_node ~now:(Ident ' ') f x

    method! var (Var v) = Var (self#handle_node ~now:(Ident (Node.contents.get v).[0]) Fun.id v)

    method! token t = self#handle_node ~now:(token_ident (Node.contents.get t)) Fun.id t

    method! literal _ l =
      let now : token_kind =
        match (Node.contents.get l.lit_node).[0] with
        | '0' .. '9' | '.' -> Number
        | '\'' | '"' -> Symbol
        | '[' -> LongString
        | _ -> failwith "Unknown literal"
      in
      l |> Literal.lit_node %= self#handle_node ~now Fun.id

    method! unop_expr { unop_op; unop_rhs } =
      let now = Node.contents.get unop_op |> UnOp.to_token |> token_ident in
      let unop_op = self#handle_node ~now Fun.id unop_op in
      let unop_rhs = self#expr unop_rhs in
      { unop_op; unop_rhs }

    method! binop_expr { binop_lhs; binop_op; binop_rhs } =
      let now = Node.contents.get binop_op |> BinOp.to_token |> token_ident in
      let binop_lhs = self#expr binop_lhs in
      let binop_op = self#handle_node ~now Fun.id binop_op in
      let binop_rhs = self#expr binop_rhs in
      { binop_lhs; binop_op; binop_rhs }
  end

let remove_trivia x = (new remove_trivia)#program x

(** Renaming replaces every (non-global) variable in the program with a fresh (hopefully shorter)
    one. There's currently no heuristics to this, so it's not going to be the {i shortest} possible
    program, but should be "good enough".

    Worth noting that we don't do any scope checking here (aside from conflicts with globals). As
    we're renaming every variable, it should be safe. *)
module Rename = struct
  module StringSet = Set.Make (String)

  type t =
    { resolve : R.t;
      keywords : StringSet.t;
      var_names : string R.VarTbl.t;
      mutable index : int
    }

  let mk resolve =
    let keywords = IlluaminateSemantics.Ident.keywords |> StringSet.add "arg" in
    let keywords =
      Seq.fold_left (fun s v -> StringSet.add v.R.name s) keywords (R.globals resolve)
    in
    { resolve; keywords; var_names = R.VarTbl.create 32; index = 0 }

  let start_chars = "etaoinshrdlucmfwypvbgkqjxzETAOINSHRDLUCMFWYPVBGKQJXZ"

  let start_chars_len = String.length start_chars

  let other_chars = "etaoinshrdlucmfwypvbgkqjxz_0123456789ETAOINSHRDLUCMFWYPVBGKQJXZ"

  let other_chars_len = String.length other_chars

  let make_name i =
    if i < start_chars_len then start_chars.[i] |> String.make 1
    else
      let buffer = Buffer.create 4 in
      Buffer.add_char buffer start_chars.[i mod start_chars_len];
      let rec go i =
        Buffer.add_char buffer other_chars.[i mod other_chars_len];
        if i > other_chars_len then go (i / other_chars_len)
      in
      go (i / start_chars_len);
      Buffer.contents buffer

  let rec get_name t : R.var -> string = function
    | { kind = Global | ImplicitArg _; name; _ } -> name
    | { kind = Arg _ | Local _ | Loop _; shadows = Some shadows; _ } -> get_name t shadows
    | { kind = Arg _ | Local _ | Loop _; shadows = None; _ } as var -> (
      match R.VarTbl.find_opt t.var_names var with
      | Some n -> n
      | None ->
          let rec go i =
            let name = make_name i in
            if StringSet.mem name t.keywords then go (i + 1) else (i, name)
          in
          let i, name = go t.index in
          t.index <- i + 1;
          R.VarTbl.add t.var_names var name;
          name )

  let rename_var t (Var name as v) =
    let n = R.get_var v t.resolve |> get_name t in
    Var (name |> Node.contents ^= n)

  class rename scopes =
    object
      inherit Syntax.map

      method! var = rename_var scopes
    end
end

let rename data program =
  let rename = IlluaminateData.need data R.key program |> Rename.mk in
  (new Rename.rename rename)#program program

let minify d = rename d % remove_trivia
