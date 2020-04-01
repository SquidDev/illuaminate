(** Matches redundant semicolons in statements.

    Statements are only required to separate function calls. For instance:

    {[
      local x = f() ; -- Semicolon required here
      (function() end)()
    ]} *)

include Linter.S
