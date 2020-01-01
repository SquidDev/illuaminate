(** Provides an interface for "linting" Lua code.

    Linters both provide warnings/errors on the source code, as well as an (optional) mechanism for
    fixing the detected problems. *)

module Linter = Linter
module Driver = Driver

type linter = Linter.t

(** The built-in linters. *)
module Linters : sig
  (** Finds assignments with pointless discard variables in the last position. *)
  val pointless_discard : linter

  (** Assignments to global variables. *)
  val set_global : linter

  (** Left/right hand side of an assignment are mismatched. *)
  val unbalanced_assign : linter

  (** Finds and fixes bindings to variables which are unused, replacing them with [_] instead. *)
  val unused : linter

  (** Warns when a term uses the [_] variable. *)
  val use_discard : linter

  (** Find and fix empty [do] and [if] statements. *)
  val empty_block : linter

  (** Matches redundant semicolons in statements.

      Statements are only required to separate function calls. For instance:

      {[
        local x = f() ; -- Semicolon required here
        (function() end)()
      ]} *)
  val pointless_semicolon : linter

  (** Find and fix redundant parenthesis. *)
  val parens : linter

  (** Errors when varargs are not at the last position in a function definition. *)
  val misplaced_dots : linter

  (** Errors on malformed function names. *)
  val method_name : linter

  (** Ensures the bounds on for-num loops makes sense. *)
  val for_num : linter

  (** Warns when mutating loop variables. *)
  val set_loop : linter

  (** Warns when defining an argument named 'arg' *)
  val arg_arg : linter

  (** Warns when using the implicit vararg 'arg' variable. *)
  val use_arg : linter

  (** Warns on unreachable code. *)
  val unreachable : linter

  (** Warns when a break is used outside a loop. *)
  val invalid_break : linter

  (** Forwards any errors from parsing doc comments. *)
  val doc_parse : linter

  (** Checks for whitespace around basic punctuation such as ",", ";", "=" and all binary operators. *)
  val spacing : linter

  (** Checks for trailing commas/semicolons on tables. *)
  val table_trailing : linter

  (** Replace string.len(x) with #x *)
  val string_len : linter

  (** Checks for invalid escape characters in strings. *)
  val string_escape : linter

  (** Checks for all {!IlluaminateCore.Syntax.MalformedNumber} nodes and informs about them. *)
  val malformed_number : linter

  (** A list of all linters. *)
  val all : linter list
end
