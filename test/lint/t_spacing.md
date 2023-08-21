See [`lint_spacing.ml`](../../src/lint/lint_spacing.ml)

# Binary operators
```lua
-- config: (lint (only format:op-space))

local x = (0+0)*0 + 1
local x = 0 + 0 -- ok
```

```txt
in.lua: Expected whitespace around "+". [format:separator-space]
   │
 3 │ local x = (0+0)*0 + 1
   │             ^

in.lua: Expected whitespace around "*". [format:separator-space]
   │
 3 │ local x = (0+0)*0 + 1
   │                ^
```

```diff
@ -2,4 +2,4 @
  -- config: (lint (only format:op-space))

- local x = (0+0)*0 + 1
+ local x = (0 + 0) * 0 + 1
  local x = 0 + 0 -- ok
```

# Around assignments

```lua
-- config: (lint (only format:op-space format:separator-space))

x = 0,1, 2
x=0
x =0
x = 0 -- ok
```

```txt
in.lua: Expected whitespace after ",". [format:separator-space]
   │
 3 │ x = 0,1, 2
   │      ^

in.lua: Expected whitespace around "=". [format:separator-space]
   │
 4 │ x=0
   │  ^

in.lua: Expected whitespace around "=". [format:separator-space]
   │
 5 │ x =0
   │   ^
```

```diff
@ -2,6 +2,6 @
  -- config: (lint (only format:op-space format:separator-space))

- x = 0,1, 2
- x=0
- x =0
+ x = 0, 1, 2
+ x = 0
+ x = 0
  x = 0 -- ok
```


# Variable and expression lists
```lua
-- config: (lint (only format:op-space format:separator-space))

local x,y, z
local x = 0,1, 2
local x=0
local x =0
local x = 0 -- ok

local x -- dubious, but ok
=
0
```

```txt
in.lua: Expected whitespace after ",". [format:separator-space]
   │
 3 │ local x,y, z
   │        ^

in.lua: Expected whitespace after ",". [format:separator-space]
   │
 4 │ local x = 0,1, 2
   │            ^

in.lua: Expected whitespace around "=". [format:separator-space]
   │
 5 │ local x=0
   │        ^

in.lua: Expected whitespace around "=". [format:separator-space]
   │
 6 │ local x =0
   │         ^
```

```diff
@ -2,9 +2,9 @
  -- config: (lint (only format:op-space format:separator-space))

- local x,y, z
- local x = 0,1, 2
- local x=0
- local x =0
+ local x, y, z
+ local x = 0, 1, 2
+ local x = 0
+ local x = 0
  local x = 0 -- ok

  local x -- dubious, but ok
```

# Arguments
```lua
-- config: (lint (only format:op-space))
print(0,1, 2)
print { x=0, 0,1, 2 }
```

```txt
in.lua: Expected whitespace after ",". [format:separator-space]
   │
 2 │ print(0,1, 2)
   │        ^

in.lua: Expected whitespace around "=". [format:separator-space]
   │
 3 │ print { x=0, 0,1, 2 }
   │          ^

in.lua: Expected whitespace after ",". [format:separator-space]
   │
 3 │ print { x=0, 0,1, 2 }
   │               ^
```

```diff
@ -1,3 +1,3 @
  -- config: (lint (only format:op-space))
- print(0,1, 2)
- print { x=0, 0,1, 2 }
+ print(0, 1, 2)
+ print { x = 0, 0, 1, 2 }
```

# Tables
```lua
-- config: (lint (only format:op-space))
local _ = { x=0 }
local _ = { x= 0 }
local _ = { x =0 }
local _ = { x = 0 } -- ok
```

```txt
in.lua: Expected whitespace around "=". [format:separator-space]
   │
 2 │ local _ = { x=0 }
   │              ^

in.lua: Expected whitespace around "=". [format:separator-space]
   │
 3 │ local _ = { x= 0 }
   │              ^

in.lua: Expected whitespace around "=". [format:separator-space]
   │
 4 │ local _ = { x =0 }
   │               ^
```

```diff
@ -1,5 +1,5 @
  -- config: (lint (only format:op-space))
- local _ = { x=0 }
- local _ = { x= 0 }
- local _ = { x =0 }
+ local _ = { x = 0 }
+ local _ = { x = 0 }
+ local _ = { x = 0 }
  local _ = { x = 0 } -- ok
```

# For loops
```lua
-- config: (lint (only format:op-space format:separator-space))

for i=0, 10 do end
for i= 0, 10 do end
for i = 0, 10 do end
for i = 0, 10 do end -- ok
```

```txt
in.lua: Expected whitespace around "=". [format:separator-space]
   │
 3 │ for i=0, 10 do end
   │      ^

in.lua: Expected whitespace around "=". [format:separator-space]
   │
 4 │ for i= 0, 10 do end
   │      ^
```

```diff
@ -2,6 +2,6 @
  -- config: (lint (only format:op-space format:separator-space))

- for i=0, 10 do end
- for i= 0, 10 do end
+ for i = 0, 10 do end
+ for i = 0, 10 do end
  for i = 0, 10 do end
  for i = 0, 10 do end -- ok
```
