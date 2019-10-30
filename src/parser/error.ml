module E = IlluaminateCore.Error

type t =
  | UnexpectedCharacter of string
  | UnterminatedString
  | SyntaxError of string
  | UnexpectedToken of Grammar.token * string

let show_error = function
  | UnexpectedCharacter c -> Printf.sprintf "Unexpected character \"%s\"" (String.escaped c)
  | UnterminatedString -> "Unterminated string"
  | SyntaxError m -> m
  | UnexpectedToken (tok, m) ->
      "Unexpected " ^ (Token.get_token tok |> IlluaminateCore.Token.show) ^ ". " ^ m

let syntax_error = E.make_tag E.Critical "parse:syntax-error"

let report errs pos err = E.report errs syntax_error pos (show_error err)
