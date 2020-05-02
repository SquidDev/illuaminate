module Linter = Linter
module Driver = Driver

type linter = Linter.t

module Linters = struct
  module Arg_arg = Lint_arg_arg
  module Bracket_spacing = Lint_bracket_spacing
  module Deprecated = Lint_deprecated
  module Detached_comment = Lint_detached_comment
  module Doc_extract = Lint_doc_extract
  module Doc_parse = Lint_doc_parse
  module Empty_block = Lint_empty_block
  module For_num = Lint_for_num
  module Invalid_break = Lint_invalid_break
  module Malformed_example = Lint_malformed_example
  module Malformed_number = Lint_malformed_number
  module Method_name = Lint_method_name
  module Misplaced_dots = Lint_misplaced_dots
  module Parens = Lint_parens
  module Pcall_eta = Lint_pcall_eta
  module Pointless_discard = Lint_pointless_discard
  module Pointless_semicolon = Lint_pointless_semicolon
  module Set_global = Lint_set_global
  module Set_loop = Lint_set_loop
  module Spacing = Lint_spacing
  module String_escape = Lint_string_escape
  module String_index = Lint_string_index
  module String_len = Lint_string_len
  module String_lib = Lint_string_lib
  module String_quote = Lint_string_quote
  module Table_separator = Lint_table_separator
  module Table_trailing = Lint_table_trailing
  module Unbalanced_assign = Lint_unbalanced_assign
  module Undocumented = Lint_undocumented
  module Unknown_global = Lint_unknown_global
  module Unreachable = Lint_unreachable
  module Unresolved_member = Lint_unresolved_member
  module Unresolved_reference = Lint_unresolved_reference
  module Unused = Lint_unused
  module Use_arg = Lint_use_arg
  module Use_discard = Lint_use_discard

  let all =
    (* TODO: Find a more efficient order for these (namely group related passes together instead) *)
    [ Arg_arg.linter;
      Bracket_spacing.linter;
      Deprecated.linter;
      Doc_parse.linter;
      Doc_extract.linter;
      Undocumented.linter;
      Unknown_global.linter;
      Unresolved_reference.linter;
      Malformed_example.linter;
      Detached_comment.linter;
      Empty_block.linter;
      For_num.linter;
      Invalid_break.linter;
      Method_name.linter;
      Misplaced_dots.linter;
      Parens.linter;
      String_escape.linter;
      Malformed_number.linter;
      String_quote.linter;
      String_index.linter;
      Pointless_semicolon.linter;
      Set_global.linter;
      Set_loop.linter;
      Spacing.linter;
      String_len.linter;
      String_lib.linter;
      Pcall_eta.linter;
      Table_separator.linter;
      Table_trailing.linter;
      Unbalanced_assign.linter;
      Unreachable.linter;
      (* "pointless_discard" occurs after "unused" to ensure we'll entirely remove redundant
         assigns. *)
      Unused.linter;
      Pointless_discard.linter;
      Use_arg.linter;
      Use_discard.linter;
      Unresolved_member.linter
    ]
end
