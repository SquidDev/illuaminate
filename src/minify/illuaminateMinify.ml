open IlluaminateCore

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
      let now = Node.contents.get unop_op |> Syntax.UnOp.to_token |> token_ident in
      let unop_op = self#handle_node ~now Fun.id unop_op in
      let unop_rhs = self#expr unop_rhs in
      { unop_op; unop_rhs }

    method! binop_expr { binop_lhs; binop_op; binop_rhs } =
      let now = Node.contents.get binop_op |> Syntax.BinOp.to_token |> token_ident in
      let binop_lhs = self#expr binop_lhs in
      let binop_op = self#handle_node ~now Fun.id binop_op in
      let binop_rhs = self#expr binop_rhs in
      { binop_lhs; binop_op; binop_rhs }
  end

let minify _ x = (new remove_trivia)#program x
