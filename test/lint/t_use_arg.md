See [`lint_use_arg.ml`](../../src/lint/lint_use_arg.ml)

```lua
-- config: (lint (only var:use-arg))

local function f(...)
  print(arg)
  arg = nil
  arg[0] = nil

  local arg = 0
  print(arg) -- ok
end
```

```txt
in.lua: Using implicit vararg variable "arg". [var:use-arg]
   │
 4 │   print(arg)
   │         ^^^

in.lua: Using implicit vararg variable "arg". [var:use-arg]
   │
 5 │   arg = nil
   │   ^^^

in.lua: Using implicit vararg variable "arg". [var:use-arg]
   │
 6 │   arg[0] = nil
   │   ^^^
```
