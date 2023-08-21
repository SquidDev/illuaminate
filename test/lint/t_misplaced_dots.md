See [`lint_misplaced_dots.ml`](../../src/lint/lint_misplaced_dots.ml)

```lua
-- config: (lint (only syntax:misplaced-dots))

local function f(..., x) end
local function f(x, ...) end -- ok
```

```txt
in.lua: Varargs can only appear as the last argument to a function. [syntax:misplaced-dots]
   │
 3 │ local function f(..., x) end
   │                  ^^^
```
