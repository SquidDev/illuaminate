module Control = Control
module Global = Global
module Pure = Pure
module Reference = Reference
module Resolve = Resolve
module Module_resolve = Module_resolve
module Namespace = Namespace

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
end
