open IlluaminateCore
open Syntax
open Lens
module R = IlluaminateSemantics.Resolve

module Emit = struct
  type t = Format.formatter

  include Emit

  let use x f = f x
end

let token_ident : Token.t -> bool = function
  | And | Break | Do | Else | ElseIf | End | False | For | Function | Ident _ | If | In | Int _
  | Local | MalformedNumber _ | Nil | Not | Number _ | Or | Repeat | Return | Then | True | Until
  | While ->
      true
  | Add | CBrace | Colon | Comma | Concat | CParen | CSquare | Div | Dot | Dots | EoF | Eq | Equals
  | Ge | Gt | Le | Len | Lt | Mod | Mul | Ne | OBrace | OParen | OSquare | Pow | Semicolon
  | String _ | Sub ->
      false

(** Removes trivia from a node. We keep track of whether the previous token was an identifier
    ([is_ident]) or other symbol which requires spaces around it (such as a number). We use this to
    ensure that [local x] is not printed as [localx], but still allow things like [f()].*)
class remove_trivia =
  object (self)
    inherit Syntax.map

    val mutable is_ident = false

    method private handle_node : 'a. now:bool -> ('a -> 'a) -> 'a Node.t -> 'a Node.t =
      fun ~now f node ->
        let open Node in
        let node =
          match node with
          | SimpleNode { contents } -> SimpleNode { contents = f contents }
          | Node { contents; span; _ } ->
              let leading =
                if is_ident && now then [ { Span.span; value = Whitespace " " } ] else []
              in
              Node { contents; span; leading_trivia = leading; trailing_trivia = [] }
        in
        is_ident <- now;
        node

    method! node f x = self#handle_node ~now:true f x

    method! token t = self#handle_node ~now:(token_ident (Node.contents.get t)) Fun.id t

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
