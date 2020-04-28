(** Warn when using an unknown global.

    Ideally this'd be merged with {!Lint_unresolved_reference}, with some complex description of
    "other" modules, but this is a good "first" approximation. *)

include Linter.S
