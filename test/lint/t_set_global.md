See [`lint_set_global.ml`](../../src/lint/lint_set_global.ml)

# Variables
```lua
-- config: (lint (only var:set-global))
local z
x, y, z = 0, 0
```

```txt
in.lua: Setting unknown global variable "x". [var:set-global]
   │
 3 │ x, y, z = 0, 0
   │ ^

in.lua: Setting unknown global variable "y". [var:set-global]
   │
 3 │ x, y, z = 0, 0
   │    ^
```

# Functions
```lua
-- config: (lint (only var:set-global))
function f() end

local g
function g() end
```

```txt
in.lua: Setting unknown global function "f". [var:set-global]
   │
 2 │ function f() end
   │          ^
```

# Allow top-level assignments
```lua
-- config: (lint (only var:set-global) (allow-toplevel-global true))
a = 0 -- ok
function b() end -- ok
if true then c = 0 end -- ok

local function f()
  d = 0
  e = 0
end

-- We need to work out what we should do in this case. Arguably it's OK as we're
-- mutating an already defined variable. But mutating API globals within a
-- function is rather bad form.
--[[
local function f()
  a = 0
  b = 0
end
]]
```

```txt
in.lua: Setting unknown global variable "d". [var:set-global]
   │
 7 │   d = 0
   │   ^

in.lua: Setting unknown global variable "e". [var:set-global]
   │
 8 │   e = 0
   │   ^
```
