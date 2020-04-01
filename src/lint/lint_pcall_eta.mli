(** Checks for calls to pcall which could be eta-reduced.

    For instance, [pcall(function() print(a, b, c) end)] can be reduced to [pcall(print, a, b, c)]. *)

include Linter.S
