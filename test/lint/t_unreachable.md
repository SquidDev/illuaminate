See [`lint_unreachable.ml`](../../src/lint/lint_unreachable.ml)

# After `do return end`
```lua
-- config: (lint (only control:unreachable))

local f = ...

local function _()
  do return end

  print()
end
```

```txt
in.lua: Unreachable code [control:unreachable]
   │
 8 │   print()
   │   ^^^^^^^
```

# After `do break end`
```lua
-- config: (lint (only control:unreachable))

local function _()
  while true do
    do break end

    print()
  end
end
```

```txt
in.lua: Unreachable code [control:unreachable]
   │
 7 │     print()
   │     ^^^^^^^
```

# `if` statements
If all statements in a `if` statement exit, then expressions afterwards are
unreachable.

```lua
-- config: (lint (only control:unreachable))

local function _()
  if f() then
    return 1
  else
    return 2
  end

  if f() then
    return 1
  else
    return 2
  end

  return
end
```

```txt
in.lua: Unreachable code [control:unreachable]
    │
 10 │   if f() then
    │   ^^^^^^^^^^^

in.lua: Unreachable code [control:unreachable]
    │
 16 │   return
    │   ^^^^^^
```

We also handle constant conditions in `if` statements.

```lua
-- config: (lint (only control:unreachable))
local function _()
  if true then
  elseif f() then
    print() -- ok
  elseif false then
    print()
  end
end
```

```txt
in.lua: Unreachable code [control:unreachable]
   │
 4 │   elseif f() then
   │          ^^^

in.lua: Unreachable code [control:unreachable]
   │
 7 │     print()
   │     ^^^^^^^
```

# `while` loops
If we have an infinite loop, the `return` won't be accessible.

```lua
-- config: (lint (only control:unreachable))

local function _()
  while true do end

  return
end
```

```txt
in.lua: Unreachable code [control:unreachable]
   │
 6 │   return
   │   ^^^^^^
```

However if we have a break, then no errors are reported.

```lua
-- config: (lint (only control:unreachable))

local function _()
  while true do
    if f() then break end
  end

  return
end
```

```txt
No errors
```

# `repeat` loops

```lua
-- config: (lint (only control:unreachable))
local function _()
  repeat
    return
  until f()
end

local function f(x)
  repeat
  until x() -- ok
end
```

```txt
in.lua: Loop is executed at most once. [control:loop-once]
   │
 3 │   repeat
   │   ^^^^^^
```
