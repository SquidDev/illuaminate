(** Provides an interface for "linting" Lua code.

    Linters both provide warnings/errors on the source code, as well as an (optional) mechanism for
    fixing the detected problems. *)

module Linter = Linter
module Driver = Driver

(** A linter, suitable for use with the linter {!Driver} *)
type linter = Linter.t

(** The built-in linters. *)
module Linters : sig
  (** A list of all linters. *)
  val all : linter list

  module Arg_arg = Lint_arg_arg
  module Bracket_spacing = Lint_bracket_spacing
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
  module Unreachable = Lint_unreachable
  module Unresolved_member = Lint_unresolved_member
  module Unresolved_reference = Lint_unresolved_reference
  module Unused = Lint_unused
  module Use_arg = Lint_use_arg
  module Use_discard = Lint_use_discard
end
