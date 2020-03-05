module E = IlluaminateCore.Error

type t =
  | UnexpectedCharacter of string
  | UnterminatedString
  | SyntaxError of string
  | UnexpectedToken of Grammar.token * string

let pp out = function
  | UnexpectedCharacter c -> Format.fprintf out "Unexpected character \"%s\"" (String.escaped c)
  | UnterminatedString -> Format.fprintf out "Unterminated string"
  | SyntaxError m -> Format.fprintf out "%s" m
  | UnexpectedToken (tok, m) ->
      Format.fprintf out "Unexpected `%a`: %s" IlluaminateCore.Token.pp (Token.get_token tok) m

let tag = E.Tag.make E.Critical "parse:syntax-error"

let report errs pos err = Format.asprintf "%a" pp err |> E.report errs tag pos
