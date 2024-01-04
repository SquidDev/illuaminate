(** All tokens which may appear within the Lua source code. *)

type t =
  | Add
  | And
  | Break
  | CBrace
  | Colon
  | Comma
  | Concat
  | CParen
  | CSquare
  | Div
  | Do
  | Dot
  | Dots
  | Double_colon
  | Else
  | ElseIf
  | End
  | EoF
  | Eq
  | Equals
  | False
  | For
  | Function
  | Ge
  | Goto
  | Gt
  | Ident of string
  | If
  | In
  | Le
  | Len
  | Local
  | Lt
  | Mod
  | Mul
  | Ne
  | Nil
  | Not
  | Number of string
  | OBrace
  | OParen
  | Or
  | OSquare
  | Pow
  | Repeat
  | Return
  | Semicolon
  | String of string
  | Sub
  | Then
  | True
  | Until
  | While

(** Convert this token into a string.

    This prints the Lua source-code version of this token, rather than its name (with the exception
    of {!EOF}) *)
let show = function
  | And -> "and"
  | Add -> "add"
  | Break -> "break"
  | CBrace -> "}"
  | Colon -> ":"
  | Comma -> ","
  | Concat -> ".."
  | CParen -> ")"
  | CSquare -> "]"
  | Div -> "/"
  | Do -> "do"
  | Dot -> "."
  | Dots -> "..."
  | Double_colon -> "::"
  | Else -> "else"
  | ElseIf -> "elseif"
  | End -> "end"
  | EoF -> "end of file"
  | Eq -> "=="
  | Equals -> "="
  | False -> "false"
  | For -> "for"
  | Function -> "function"
  | Ge -> ">="
  | Goto -> "goto"
  | Gt -> ">"
  | Ident x -> x
  | If -> "if"
  | In -> "in"
  | Le -> "<="
  | Len -> "#"
  | Local -> "local"
  | Lt -> "<"
  | Mod -> "%"
  | Mul -> "*"
  | Ne -> "~="
  | Nil -> "nil"
  | Not -> "not"
  | Number x -> x
  | OBrace -> "{"
  | OParen -> "("
  | Or -> "or"
  | OSquare -> "["
  | Pow -> "^"
  | Repeat -> "repeat"
  | Return -> "return"
  | Semicolon -> ";"
  | String x -> x
  | Sub -> "-"
  | Then -> "then"
  | True -> "true"
  | Until -> "until"
  | While -> "while"

(** Print this token to a formatter.

    This is equivalent to printing {!show} to the formatter. *)
let pp out tok = Format.pp_print_string out (show tok)
