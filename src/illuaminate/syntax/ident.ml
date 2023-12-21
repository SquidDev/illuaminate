module StringSet = Set.Make (String)

let keywords =
  StringSet.of_list
    [ "and";
      "break";
      "do";
      "else";
      "elseif";
      "end";
      "false";
      "for";
      "function";
      "if";
      "in";
      "local";
      "nil";
      "not";
      "or";
      "repeat";
      "return";
      "then";
      "true";
      "until";
      "while"
    ]

let is_start = function
  | '_' | 'A' .. 'Z' | 'a' .. 'z' -> true
  | _ -> false

let is_rest = function
  | '_' | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' -> true
  | _ -> false

let is x =
  String.length x > 0
  && is_start x.[0]
  && CCString.for_all is_rest x
  && not (StringSet.mem x keywords)
