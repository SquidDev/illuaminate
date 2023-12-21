(** Components for literal strings, see {!Literal.String}. *)

exception Malformed

type component =
  | Quote of char
  | Segment
  | Escape of char
  | Decimal_escape of char
  | Hex_escape of char
  | Unicode_escape of Uchar.t
  | Unknown_escape of char
  | Zap

let parse_hex = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
  | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
  | _ -> invalid_arg "Not a hexadecimal character"

let hex_escape l r = Hex_escape (Char.chr ((parse_hex l lsl 4) lor parse_hex r))

let decimal_escape i =
  let i = int_of_string i in
  if i > 255 then raise Malformed else Decimal_escape (Char.chr i)

let unicode_escape i =
  let i = int_of_string ("0x" ^ i) in
  if i > 0x10ffff then raise Malformed else Unicode_escape (Uchar.of_int i)
