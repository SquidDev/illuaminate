See [`lint_unresolved_member.ml`](../../src/lint/lint_unresolved_member.ml)

```lua
-- config: (lint (only var:unresolved-member))

--- This is a documented term
local tbl_1 = { x = 1 }

-- This is not documented
local tbl_2 = { x = 1 }

--- This is also documented
local function not_tbl() end

print(tbl_1.x, tbl_1.y)
print(tbl_2.x, tbl_2.y) -- Doesn't warn as not documented
print(not_tbl.x) -- Doesn't warn as not table


for i = 1, (function()
  local tbl_3 = { x = 1 } --- Documented
  print(tbl_3.y)
end)() do end

do
  local tbl_4 = { x = 1 } --- Documented
  print(tbl_4.y)
end

local a_module = require("a_module")
print(a_module.a) --- Documented
print(a_module.unknown) --- Undocumented
```

```txt
in.lua: Unknown field y in tbl_1 [var:unresolved-member]
    │
 12 │ print(tbl_1.x, tbl_1.y)
    │                ^^^^^^^

in.lua: Unknown field y in tbl_3 [var:unresolved-member]
    │
 19 │   print(tbl_3.y)
    │         ^^^^^^^

in.lua: Unknown field y in tbl_4 [var:unresolved-member]
    │
 24 │   print(tbl_4.y)
    │         ^^^^^^^

in.lua: Unknown field unknown in a_module [var:unresolved-member]
    │
 29 │ print(a_module.unknown) --- Undocumented
    │       ^^^^^^^^^^^^^^^^
```

# Supports "dynamic" modules
Some modules can be marked as "dynamic", effectively excluding them from this check.
```lua
-- config: (lint (only var:unresolved-member) (dynamic-modules a_module))

local a_module, b_module = require("a_module"), require("b_module")
print(a_module.unknown)
print(b_module.unknown)
```

```txt
in.lua: Unknown field unknown in b_module [var:unresolved-member]
   │
 5 │ print(b_module.unknown)
   │       ^^^^^^^^^^^^^^^^
```
