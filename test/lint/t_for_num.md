See [`lint_for_num.ml`](../../src/lint/lint_for_num.ml)

# Counting up
```lua
-- config: (lint (only control:for-num))

for _ = 1, 10 do end -- ok
for _ = 1, 10, 1 do end -- ok
for _ = 1, 10, -1 do end
```

```txt
in.lua: Numeric for loop counts up from 1 to 10, but has a negative step. [control:for-num]
   │
 5 │ for _ = 1, 10, -1 do end
   │ ^^^^^^^^^^^^^^^^^^^^^^^^
```

# Counting nowhere
```lua
-- config: (lint (only control:for-num))

for _ = 1, 10, 0 do end
```

```txt
in.lua: Numeric for loop has a step of 0. It will never progress. [control:for-num]
   │
 3 │ for _ = 1, 10, 0 do end
   │ ^^^^^^^^^^^^^^^^^^^^^^^
```

# Counting down

```lua
-- config: (lint (only control:for-num))

for _ = 1, -10 do end
for _ = 1, -10, -1 do end -- ok
```

```txt
in.lua: Numeric for loop counts down from 1 to -10, but has a non-negative step. [control:for-num]
   │
 3 │ for _ = 1, -10 do end
   │ ^^^^^^^^^^^^^^^^^^^^^
```

# Counting down on tables

```lua
-- config: (lint (only control:for-num))

local t, x = ...
for _ = #t, 1, -1 do end -- ok
for _ = #t, 1, x do end -- ok
for _ = #t, 1 do end
```

```txt
in.lua: Numeric for loop counts down from #(expr) to 1, but has a non-negative step. [control:for-num]
   │
 6 │ for _ = #t, 1 do end
   │ ^^^^^^^^^^^^^^^^^^^^
```

# Empty range
```lua
-- config: (lint (only control:for-num))

for _ = 1, 1 do end
for _ = 1 + 4, 5 do end
```

```txt
in.lua: This loop has the same start and stop point, and so will only execute once. [control:for-num]
   │
 3 │ for _ = 1, 1 do end
   │ ^^^^^^^^^^^^^^^^^^^

in.lua: This loop has the same start and stop point, and so will only execute once. [control:for-num]
   │
 4 │ for _ = 1 + 4, 5 do end
   │ ^^^^^^^^^^^^^^^^^^^^^^^
```
