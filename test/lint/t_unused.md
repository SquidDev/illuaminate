See [`lint_unused.ml`](../../src/lint/lint_unused.ml)

# Unused variables

```lua
-- config: (lint (only var:unused))
local x, y, z, _ = 0, 0, 0
print(x)

y = 0
z[0] = 0

global = 0

-- FIXME: y = 0 should not warn for unused var (unused val? Sure)
-- TODO: Is z bound or not?
```

```txt
in.lua: Unused variable "y". [var:unused]
   │
 2 │ local x, y, z, _ = 0, 0, 0
   │          ^
in.lua: Unused variable "y". [var:unused]
   │
 5 │ y = 0
   │ ^

in.lua: Unused variable "global". [var:unused-global]
   │
 8 │ global = 0
   │ ^^^^^^
```

```diff
@ -1,11 +1,11 @
  -- config: (lint (only var:unused))
- local x, y, z, _ = 0, 0, 0
+ local x, _, z, _ = 0, 0, 0
  print(x)

- y = 0
+ _ = 0
  z[0] = 0

- global = 0
+ _ = 0

  -- FIXME: y = 0 should not warn for unused var (unused val? Sure)
  -- TODO: Is z bound or not?
```

# Unused dots
```lua
-- config: (lint (only var:unused))

local function f(x, ...) return x end
local function g(...) end

local function h(...) return ... end -- ok
local function i(...) return arg end -- ok

return { f, g, h, i }
```

```txt
in.lua: Unused varargs. [var:unused-arg]
   │
 3 │ local function f(x, ...) return x end
   │                     ^^^

in.lua: Unused varargs. [var:unused-arg]
   │
 4 │ local function g(...) end
   │                  ^^^
```

```diff
@ -2,7 +2,7 @
  -- config: (lint (only var:unused))

- local function f(x, ...) return x end
- local function g(...) end
+ local function f(x) return x end
+ local function g() end

  local function h(...) return ... end -- ok
  local function i(...) return arg end -- ok
```
