See [`lint_invalid_break.ml`](../../src/lint/lint_invalid_break.ml)

```lua
-- config: (lint (only syntax:invalid-break))

break

while true do
  break -- ok

  local function f()
    break
  end
end
```

```txt
in.lua: Using `break` outside a loop [syntax:invalid-break]
   │
 3 │ break
   │ ^^^^^

in.lua: Using `break` outside a loop [syntax:invalid-break]
   │
 9 │     break
   │     ^^^^^
```
