(** Represents any kind of token *)

open IlluaminateCore
open Node
open Syntax.UnOp
open Syntax.BinOp
open Token
open Illuaminate.Lens

type lexer_token =
  | Token of IlluaminateCore.Token.t
  | Trivial of IlluaminateCore.Node.trivial

let make_token leading_trivia trailing_trivia span : IlluaminateCore.Token.t -> Grammar.token =
  let mk contents = Node { leading_trivia; trailing_trivia; span; contents } in
  function
  | Add -> ADD (mk OpAdd)
  | And -> AND (mk OpAnd)
  | Break -> BREAK (mk Token.Break)
  | CBrace -> CBRACE (mk CBrace)
  | Colon -> COLON (mk Colon)
  | Comma -> COMMA (mk Comma)
  | Concat -> CONCAT (mk OpConcat)
  | CParen -> CPAREN (mk CParen)
  | CSquare -> CSQUARE (mk CSquare)
  | Div -> DIV (mk OpDiv)
  | Do -> DO (mk Token.Do)
  | Dot -> DOT (mk Dot)
  | Dots -> DOTS (mk Token.Dots)
  | Else -> ELSE (mk Else)
  | ElseIf -> ELSEIF (mk ElseIf)
  | End -> END (mk End)
  | EoF -> EOF (mk Token.EoF)
  | Eq -> EQ (mk OpEq)
  | Equals -> EQUALS (mk Equals)
  | False -> FALSE (mk Token.False)
  | For -> FOR (mk For)
  | Function -> FUNCTION (mk Function)
  | Goto -> GOTO (mk Token.Goto)
  | Double_colon -> DOUBLE_COLON (mk Double_colon)
  | Ge -> GE (mk OpGe)
  | Gt -> GT (mk OpGt)
  | If -> IF (mk Token.If)
  | In -> IN (mk In)
  | Le -> LE (mk OpLe)
  | Len -> LEN (mk OpLen)
  | Local -> LOCAL (mk Token.Local)
  | Lt -> LT (mk OpLt)
  | Mod -> MOD (mk OpMod)
  | Mul -> MUL (mk OpMul)
  | Ne -> NE (mk OpNe)
  | Nil -> NIL (mk Token.Nil)
  | Not -> NOT (mk OpNot)
  | OBrace -> OBRACE (mk OBrace)
  | OParen -> OPAREN (mk OParen)
  | Or -> OR (mk OpOr)
  | OSquare -> OSQUARE (mk OSquare)
  | Pow -> POW (mk OpPow)
  | Repeat -> REPEAT (mk Token.Repeat)
  | Return -> RETURN (mk Token.Return)
  | Semicolon -> SEMICOLON (mk Token.Semicolon)
  | Sub -> SUB (mk OpSub)
  | Then -> THEN (mk Then)
  | True -> TRUE (mk Token.True)
  | Until -> UNTIL (mk Until)
  | While -> WHILE (mk Token.While)
  | Ident x -> IDENT (mk x)
  | String x -> STRING (mk x)
  | Number x -> NUMBER (mk x)

let to_string : Grammar.token -> string = function
  (* Keywords and symbols *)
  | ADD _ -> "add"
  | AND _ -> "and"
  | BREAK _ -> "break"
  | CBRACE _ -> "}"
  | COLON _ -> ":"
  | COMMA _ -> ","
  | CONCAT _ -> ".."
  | CPAREN _ -> ")"
  | CSQUARE _ -> "]"
  | DIV _ -> "/"
  | DO _ -> "do"
  | DOT _ -> "."
  | DOTS _ -> "..."
  | DOUBLE_COLON _ -> "::"
  | ELSE _ -> "else"
  | ELSEIF _ -> "elseif"
  | END _ -> "end"
  | EQ _ -> "=="
  | EQUALS _ -> "="
  | FALSE _ -> "false"
  | FOR _ -> "for"
  | FUNCTION _ -> "function"
  | GE _ -> ">="
  | GOTO _ -> "goto"
  | GT _ -> ">"
  | IF _ -> "if"
  | IN _ -> "in"
  | LE _ -> "<="
  | LEN _ -> "#"
  | LOCAL _ -> "local"
  | LT _ -> "<"
  | MOD _ -> "%"
  | MUL _ -> "*"
  | NE _ -> "~="
  | NIL _ -> "nil"
  | NOT _ -> "not"
  | OBRACE _ -> "{"
  | OPAREN _ -> "("
  | OR _ -> "or"
  | OSQUARE _ -> "["
  | POW _ -> "^"
  | REPEAT _ -> "repeat"
  | RETURN _ -> "return"
  | SEMICOLON _ -> ";"
  | SUB _ -> "-"
  | THEN _ -> "then"
  | TRUE _ -> "true"
  | UNTIL _ -> "until"
  | WHILE _ -> "while"
  (* Special tokens. *)
  | EOF _ -> "<eof>"
  | IDENT x | STRING x | NUMBER x -> x ^. contents
