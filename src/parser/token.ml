open struct
  module Node = IlluaminateCore.Node
end

(** Tokens produced by the lexer.

    Unlike the parser's tokens ({!Grammar.token}), these carry minimal metadata. *)
type lexer_token =
  (* Symbols *)
  | BREAK
  | DO
  | ELSE
  | ELSEIF
  | END
  | FALSE
  | FOR
  | FUNCTION
  | GOTO
  | IF
  | IN
  | LOCAL
  | NIL
  | REPEAT
  | RETURN
  | THEN
  | TRUE
  | UNTIL
  | WHILE
  | COLON
  | COMMA
  | DOT
  | DOTS
  | DOUBLE_COLON
  | EQUALS
  | SEMICOLON
  | OPAREN
  | CPAREN
  | OBRACE
  | CBRACE
  | OSQUARE
  | CSQUARE
  | ADD
  | SUB
  | MUL
  | DIV
  | POW
  | MOD
  | CONCAT
  | EQ
  | NE
  | LT
  | LE
  | GT
  | GE
  | LEN
  | AND
  | OR
  | NOT
  (* Metadata carrying *)
  | IDENT of string
  | STRING of string
  | NUMBER of string
  (* Trivia *)
  | TRIVIA of Node.Trivia.kind * string
  | EOF

let to_string : Grammar.token -> string = function
  (* Keywords *)
  | BREAK _ -> "break"
  | DO _ -> "do"
  | ELSE _ -> "else"
  | GOTO _ -> "goto"
  | ELSEIF _ -> "elseif"
  | END _ -> "end"
  | FALSE _ -> "false"
  | FOR _ -> "for"
  | FUNCTION _ -> "function"
  | IF _ -> "if"
  | IN _ -> "in"
  | LOCAL _ -> "local"
  | NIL _ -> "nil"
  | REPEAT _ -> "repeat"
  | RETURN _ -> "return"
  | THEN _ -> "then"
  | TRUE _ -> "true"
  | UNTIL _ -> "until"
  | WHILE _ -> "while"
  (* Symbols *)
  | COLON _ -> ":"
  | COMMA _ -> ","
  | DOT _ -> "."
  | DOTS _ -> "..."
  | DOUBLE_COLON _ -> "::"
  | EQUALS _ -> "="
  | SEMICOLON _ -> ";"
  | OPAREN _ -> "("
  | CPAREN _ -> ")"
  | OBRACE _ -> "{"
  | CBRACE _ -> "}"
  | OSQUARE _ -> "["
  | CSQUARE _ -> "]"
  | ADD _ -> "+"
  | SUB _ -> "-"
  | MUL _ -> "*"
  | DIV _ -> "/"
  | POW _ -> "^"
  | MOD _ -> "%"
  | CONCAT _ -> ".."
  | EQ _ -> "=="
  | NE _ -> "~="
  | LT _ -> "<"
  | LE _ -> "<="
  | GT _ -> ">"
  | GE _ -> ">="
  | LEN _ -> "#"
  (* Keyword operators *)
  | AND _ -> "and"
  | OR _ -> "or"
  | NOT _ -> "not"
  (* Metadata carrying *)
  | IDENT x -> Node.contents.get x
  | STRING x -> Node.contents.get x
  | NUMBER x -> Node.contents.get x
  | EOF _ -> "end of file"

let mk ~leading_trivia ~trailing_trivia ~span contents : _ Node.t =
  { leading_trivia; trailing_trivia; span; contents }

(** Convert the token to a parser token.

    This converts the token to a syntax node, and then boxes it into a {!Parser.token}. *)
let make_token ~leading_trivia ~trailing_trivia ~span t : Grammar.token =
  let open IlluaminateCore.Token in
  let open IlluaminateCore.Syntax.BinOp in
  let open IlluaminateCore.Syntax.UnOp in
  match t with
  (* Keywords *)
  | BREAK -> BREAK (mk ~leading_trivia ~trailing_trivia ~span Break)
  | DO -> DO (mk ~leading_trivia ~trailing_trivia ~span Do)
  | ELSE -> ELSE (mk ~leading_trivia ~trailing_trivia ~span Else)
  | ELSEIF -> ELSEIF (mk ~leading_trivia ~trailing_trivia ~span ElseIf)
  | END -> END (mk ~leading_trivia ~trailing_trivia ~span End)
  | FALSE -> FALSE (mk ~leading_trivia ~trailing_trivia ~span False)
  | FOR -> FOR (mk ~leading_trivia ~trailing_trivia ~span For)
  | FUNCTION -> FUNCTION (mk ~leading_trivia ~trailing_trivia ~span Function)
  | GOTO -> GOTO (mk ~leading_trivia ~trailing_trivia ~span Goto)
  | IF -> IF (mk ~leading_trivia ~trailing_trivia ~span If)
  | IN -> IN (mk ~leading_trivia ~trailing_trivia ~span In)
  | LOCAL -> LOCAL (mk ~leading_trivia ~trailing_trivia ~span Local)
  | NIL -> NIL (mk ~leading_trivia ~trailing_trivia ~span Nil)
  | REPEAT -> REPEAT (mk ~leading_trivia ~trailing_trivia ~span Repeat)
  | RETURN -> RETURN (mk ~leading_trivia ~trailing_trivia ~span Return)
  | THEN -> THEN (mk ~leading_trivia ~trailing_trivia ~span Then)
  | TRUE -> TRUE (mk ~leading_trivia ~trailing_trivia ~span True)
  | UNTIL -> UNTIL (mk ~leading_trivia ~trailing_trivia ~span Until)
  | WHILE -> WHILE (mk ~leading_trivia ~trailing_trivia ~span While)
  (* Symbols *)
  | COLON -> COLON (mk ~leading_trivia ~trailing_trivia ~span Colon)
  | DOUBLE_COLON -> DOUBLE_COLON (mk ~leading_trivia ~trailing_trivia ~span Double_colon)
  | COMMA -> COMMA (mk ~leading_trivia ~trailing_trivia ~span Comma)
  | DOT -> DOT (mk ~leading_trivia ~trailing_trivia ~span Dot)
  | DOTS -> DOTS (mk ~leading_trivia ~trailing_trivia ~span Dots)
  | EQUALS -> EQUALS (mk ~leading_trivia ~trailing_trivia ~span Equals)
  | SEMICOLON -> SEMICOLON (mk ~leading_trivia ~trailing_trivia ~span Semicolon)
  | OPAREN -> OPAREN (mk ~leading_trivia ~trailing_trivia ~span OParen)
  | CPAREN -> CPAREN (mk ~leading_trivia ~trailing_trivia ~span CParen)
  | OBRACE -> OBRACE (mk ~leading_trivia ~trailing_trivia ~span OBrace)
  | CBRACE -> CBRACE (mk ~leading_trivia ~trailing_trivia ~span CBrace)
  | OSQUARE -> OSQUARE (mk ~leading_trivia ~trailing_trivia ~span OSquare)
  | CSQUARE -> CSQUARE (mk ~leading_trivia ~trailing_trivia ~span CSquare)
  | ADD -> ADD (mk ~leading_trivia ~trailing_trivia ~span OpAdd)
  | SUB -> SUB (mk ~leading_trivia ~trailing_trivia ~span OpSub)
  | MUL -> MUL (mk ~leading_trivia ~trailing_trivia ~span OpMul)
  | DIV -> DIV (mk ~leading_trivia ~trailing_trivia ~span OpDiv)
  | POW -> POW (mk ~leading_trivia ~trailing_trivia ~span OpPow)
  | MOD -> MOD (mk ~leading_trivia ~trailing_trivia ~span OpMod)
  | CONCAT -> CONCAT (mk ~leading_trivia ~trailing_trivia ~span OpConcat)
  | EQ -> EQ (mk ~leading_trivia ~trailing_trivia ~span OpEq)
  | NE -> NE (mk ~leading_trivia ~trailing_trivia ~span OpNe)
  | LT -> LT (mk ~leading_trivia ~trailing_trivia ~span OpLt)
  | LE -> LE (mk ~leading_trivia ~trailing_trivia ~span OpLe)
  | GT -> GT (mk ~leading_trivia ~trailing_trivia ~span OpGt)
  | GE -> GE (mk ~leading_trivia ~trailing_trivia ~span OpGe)
  | LEN -> LEN (mk ~leading_trivia ~trailing_trivia ~span OpLen)
  (* Keyword operators *)
  | AND -> AND (mk ~leading_trivia ~trailing_trivia ~span OpAnd)
  | OR -> OR (mk ~leading_trivia ~trailing_trivia ~span OpOr)
  | NOT -> NOT (mk ~leading_trivia ~trailing_trivia ~span OpNot)
  (* Metadata carrying *)
  | IDENT x -> IDENT (mk ~leading_trivia ~trailing_trivia ~span x)
  | STRING x -> STRING (mk ~leading_trivia ~trailing_trivia ~span x)
  | NUMBER x -> NUMBER (mk ~leading_trivia ~trailing_trivia ~span x)
  (* Trivia *)
  | TRIVIA _ -> invalid_arg "Cannot create token of trivia"
  | EOF -> EOF (mk ~leading_trivia ~trailing_trivia ~span EoF)
