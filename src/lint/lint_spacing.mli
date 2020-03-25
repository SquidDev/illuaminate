(** Checks for whitespace around basic punctuation such as ",", ";", "=" and all binary operators.

    Note that this is rather un-configurable right now, as it is either "on" or "off". Ideally this
    would be more configurable in the future.

    {1 TODO: Future improvements}

    - Ensure consistent spacing at start/end of tables, calls and function definitions ([{foo }]
      should be [{foo}] or [{ foo }]).
    - Related to the above, but enforce a specific kind of table, call and function def style.
    - Make this more extensible, so we can configure it for other tokens too. *)

include Linter.S
