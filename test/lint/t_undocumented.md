See [`lint_undocumented.ml`](../../src/lint/lint_undocumented.ml)

```lua
--- config: (lint (only doc:undocumented))
-- Foo
-- @module

--- @type Foo
local Foo = {}

return {
  has_docs = function() end, --- This has documentation

  no_docs = function() end, -- This doesn't

  --- Has description
  no_args = function(a, b) end,

  --- Has description
  -- @param a Has description
  -- @param b
  no_arg_desc = function(a, b) end,

}
```

```txt
in.lua: Type 'Foo' is exported, but has no documentation. [doc:undocumented]
   │
 6 │ local Foo = {}
   │             ^^

in.lua: `no_docs` is exported, but has no documentation. [doc:undocumented]
    │
 11 │   no_docs = function() end, -- This doesn't
    │             ^^^^^^^^^^^^^^

in.lua: Argument `b` is missing a description [doc:undocumented-arg]
    │
 14 │   no_args = function(a, b) end,
    │             ^^^^^^^^^^^^^^^^^^

in.lua: Argument `a` is missing a description [doc:undocumented-arg]
    │
 14 │   no_args = function(a, b) end,
    │             ^^^^^^^^^^^^^^^^^^

in.lua: Argument `b` is missing a description [doc:undocumented-arg]
    │
 19 │   no_arg_desc = function(a, b) end,
    │                 ^^^^^^^^^^^^^^^^^^
```

# Modules
Undocumented modules print a warning

```lua
--- @module f

return 0

-- config: (lint (only doc:undocumented))
```

```txt
in.lua: Module is missing documentation [doc:undocumented]
   │
 3 │ return 0
   │        ^
```

But if we use the same name as the other module, this is fine.

```lua
--- @module a_module

-- This is OK as a_module is documented elsewhere

return 0

-- config: (lint (only doc:undocumented))
```

```txt
No errors
```
