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

  (** Forwards any errors from extracting doc comments. *)
  val doc_extract : linter

  (** Warns on undocumented nodes. *)
  val undocumented : linter

  (** Warns on unknown references within doc comments. *)
  val unknown_reference : linter

  (** Checks for whitespace around basic punctuation such as ",", ";", "=" and all binary operators. *)
  val spacing : linter

  (** Checks for trailing commas/semicolons on tables. *)
  val table_trailing : linter

  (** Replace string.len(x) with #x *)
  val string_len : linter

  (** Warn about various issues with the string library *)
  val string_lib : linter

  (** Checks for invalid escape characters in strings. *)
  val string_escape : linter

  (** Checks for all {!IlluaminateCore.Syntax.MalformedNumber} nodes and informs about them. *)
  val malformed_number : linter

  (** Checks for calls to pcall which could be eta-reduced. *)
  val pcall_eta : linter

  (** Warn when using an unresolved member of a module or table *)
  val unresolved_member : linter
end
