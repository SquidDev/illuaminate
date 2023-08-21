See [`lint_unresolved_reference.ml`](../../src/lint/lint_unresolved_reference.ml)

```lua
--- config: (lint (only doc:unresolved-reference))
-- Foo
-- @module x

--- @type Foo
local Foo = {}

return {
  --- A small test
  --
  -- See a @{UnknownDescription}
  --
  -- @tparam UnknownArgType f A description
  -- @treturn UnknownRetType Description
  -- @see UnknownSee
  -- @see Foo This and @{has_docs} should be resolved
  -- @usage See @{UnknownExample}
  -- ```lua
  -- Foo
  -- ```
  has_docs = function(f) end,
}
```

```txt
in.lua: Unknown reference "UnknownDescription". [doc:unresolved-reference]
   │
 9 │   --- A small test
   │       ^^^^^^^^^^^^

in.lua: Unknown reference "UnknownSee". [doc:unresolved-reference]
    │
 15 │   -- @see UnknownSee
    │           ^^^^^^^^^^

in.lua: Unknown reference "UnknownExample". [doc:unresolved-reference]
    │
 17 │   -- @usage See @{UnknownExample}
    │             ^^^^^^^^^^^^^^^^^^^^^

in.lua: Unknown type "UnknownRetType". [doc:unresolved-reference]
    │
 21 │   has_docs = function(f) end,
    │              ^^^^^^^^^^^^^^^

in.lua: Unknown type "UnknownArgType". [doc:unresolved-reference]
    │
 21 │   has_docs = function(f) end,
    │              ^^^^^^^^^^^^^^^
```
