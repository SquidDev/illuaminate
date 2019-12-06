module Linter = Linter
module Driver = Driver

type linter = Linter.t

module Linters = struct
  let pointless_discard = Lint_pointless_discard.linter

  let set_global = Lint_set_global.linter

  let unbalanced_assign = Lint_unbalanced_assign.linter

  let unused = Lint_unused.linter

  let use_discard = Lint_use_discard.linter

  let empty_block = Lint_empty_block.linter

  let pointless_semicolon = Lint_pointless_semicolon.linter

  let parens = Lint_parens.linter

  let misplaced_dots = Lint_misplaced_dots.linter

  let method_name = Lint_method_name.linter

  let for_num = Lint_for_num.linter

  let set_loop = Lint_set_loop.linter

  let arg_arg = Lint_arg_arg.linter

  let use_arg = Lint_use_arg.linter

  let unreachable = Lint_unreachable.linter

  let invalid_break = Lint_invalid_break.linter

  let doc_parse = Lint_doc_parse.linter

  let spacing = Lint_spacing.linter

  let all =
    [ arg_arg;
      doc_parse;
      empty_block;
      for_num;
      invalid_break;
      method_name;
      misplaced_dots;
      parens;
      pointless_semicolon;
      set_global;
      set_loop;
      spacing;
      unbalanced_assign;
      unreachable;
      (* "pointless_discard" occurs after "unused" to ensure we'll entirely remove redundant
         assigns. *)
      unused;
      pointless_discard;
      use_arg;
      use_discard
    ]
end
