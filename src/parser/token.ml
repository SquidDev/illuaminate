(** Represents any kind of token *)

open IlluaminateCore
open Node
open Syntax.UnOp
open Syntax.BinOp
open! Token
open! Grammar
open Lens

type lexer_token =
  | Token of Token.t
  | Trivial of trivial

let make_token leading_trivia trailing_trivia span =
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

let get_token = function
  | ADD _ -> Add
  | AND _ -> And
  | BREAK _ -> Break
  | CBRACE _ -> CBrace
  | COLON _ -> Colon
  | COMMA _ -> Comma
  | CONCAT _ -> Concat
  | CPAREN _ -> CParen
  | CSQUARE _ -> CSquare
  | DIV _ -> Div
  | DO _ -> Do
  | DOT _ -> Dot
  | DOTS _ -> Dots
  | DOUBLE_COLON _ -> Double_colon
  | ELSE _ -> Else
  | ELSEIF _ -> ElseIf
  | END _ -> End
  | EOF _ -> EoF
  | EQ _ -> Eq
  | EQUALS _ -> Equals
  | FALSE _ -> False
  | FOR _ -> For
  | FUNCTION _ -> Function
  | GE _ -> Ge
  | GOTO _ -> Goto
  | GT _ -> Gt
  | IF _ -> If
  | IN _ -> In
  | LE _ -> Le
  | LEN _ -> Len
  | LOCAL _ -> Local
  | LT _ -> Lt
  | MOD _ -> Mod
  | MUL _ -> Mul
  | NE _ -> Ne
  | NIL _ -> Nil
  | NOT _ -> Not
  | OBRACE _ -> OBrace
  | OPAREN _ -> OParen
  | OR _ -> Or
  | OSQUARE _ -> OSquare
  | POW _ -> Pow
  | REPEAT _ -> Repeat
  | RETURN _ -> Return
  | SEMICOLON _ -> Semicolon
  | SUB _ -> Sub
  | THEN _ -> Then
  | TRUE _ -> True
  | UNTIL _ -> Until
  | WHILE _ -> While
  | IDENT x -> Ident (x ^. contents)
  | STRING x -> String (x ^. contents)
  | NUMBER x -> Number (x ^. contents)

let get_span =
  let get_span = function
    | Node.Node { span; _ } -> span
    | _ -> assert false
  in
  function
  | ADD x
  | AND x
  | CONCAT x
  | DIV x
  | EQ x
  | GE x
  | GT x
  | LE x
  | LT x
  | MOD x
  | MUL x
  | NE x
  | OR x
  | POW x
  | SUB x -> get_span x
  | LEN x | NOT x -> get_span x
  | BREAK x
  | CBRACE x
  | COLON x
  | COMMA x
  | CPAREN x
  | CSQUARE x
  | DO x
  | DOT x
  | DOTS x
  | DOUBLE_COLON x
  | ELSE x
  | ELSEIF x
  | END x
  | EOF x
  | EQUALS x
  | FALSE x
  | FOR x
  | FUNCTION x
  | GOTO x
  | IF x
  | IN x
  | LOCAL x
  | NIL x
  | OBRACE x
  | OPAREN x
  | OSQUARE x
  | REPEAT x
  | RETURN x
  | SEMICOLON x
  | THEN x
  | TRUE x
  | UNTIL x
  | WHILE x -> get_span x
  | IDENT x | STRING x | NUMBER x -> get_span x
