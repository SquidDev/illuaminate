See [`lint_deprecated.ml`](../../src/lint/lint_deprecated.ml)

```lua
--- config: (lint (only var:deprecated))

--- @deprecated
local x

--- @deprecated Use something else instead
local y

x = {} -- Assignments to deprecated members are OK.

-- Usages are not.
x.a = 1
print(x, y)
```

```txt
in.lua: Using deprecated member. [var:deprecated]
    │
 12 │ x.a = 1
    │ ^

in.lua: Using deprecated member. [var:deprecated]
    │
 13 │ print(x, y)
    │       ^

in.lua: Using deprecated member. [var:deprecated]
    │
 13 │ print(x, y)
    │          ^
Use something else instead
```
