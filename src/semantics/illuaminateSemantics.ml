module Control = Control
module Global = Global
module Pure = Pure
module Reference = Reference
module Resolve = Resolve
module Module_resolve = Module_resolve

module Doc = struct
  module AbstractSyntax = Doc_abstract_syntax
  module Comment = Doc_comment
  module Parser = Doc_parser
  module Syntax = Doc_syntax
  module Extract = Doc_extract
end

module Type = struct
  module Syntax = Type_syntax
end

module Stringlib = struct
  module Format = struct
    include String_format

    type t = specifier list

    let parse str = Lexing.from_string str |> format []
  end

  module Literal = struct
    open IlluaminateCore
    include String_escape

    type t = component list

    let parse node = Lexing.from_string (Node.contents.get node) |> string_of (Node.span node)
  end
end

module Ident = struct
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
end
