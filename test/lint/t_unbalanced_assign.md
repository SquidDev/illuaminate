See [`lint_unbalanced_assign.ml`](../../src/lint/lint_unbalanced_assign.ml)

```lua
-- config: (lint (only var:unbalanced-assign))

local x, y = 0 -- ok
local x, y = 0, 0, 0

x, y = 0
x, y = 0, 0, 0
x, y = ...
```

```txt
in.lua: Right-hand side of assignment has more values than left hand side expects. [var:unbalanced-assign]
   │
 4 │ local x, y = 0, 0, 0
   │ ^^^^^^^^^^^^^^^^^^^^

in.lua: Right-hand side of assignment has less values than left hand side expects. [var:unbalanced-assign]
   │
 6 │ x, y = 0
   │ ^^^^^^^^

in.lua: Right-hand side of assignment has more values than left hand side expects. [var:unbalanced-assign]
   │
 7 │ x, y = 0, 0, 0
   │ ^^^^^^^^
```
