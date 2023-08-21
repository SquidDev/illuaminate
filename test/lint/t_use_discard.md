See [`lint_use_discard.ml`](../../src/lint/lint_use_discard.ml)

```lua
-- config: (lint (only var:use-discard))
local _, x = print()

print(_)
x[_] = 0
```

```txt
in.lua: Using "discard" variable `_`. [var:use-discard]
   │
 4 │ print(_)
   │       ^

in.lua: Using "discard" variable `_`. [var:use-discard]
   │
 5 │ x[_] = 0
   │   ^
```
