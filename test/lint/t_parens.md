See [`lint_parens.ml`](../../src/lint/lint_parens.ml)

# Redundant parenthesis around references
```lua
-- config: (lint (only syntax:redundant-parens))
(f)((f), {
  (f)
})
```

```txt
in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
   │
 2 │ (f)((f), {
   │ ^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
   │
 2 │ (f)((f), {
   │     ^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
   │
 3 │   (f)
   │   ^^^
```

```diff
@ -1,4 +1,4 @
  -- config: (lint (only syntax:redundant-parens))
- (f)((f), {
-   (f)
+ f(f, {
+   f
  })
```

# Redundant parenthesis around literals
```lua
-- config: (lint (only syntax:redundant-parens))
(nil)() ; -- ok
((nil))()
local _ = (nil)[1] -- ok
local _ = ((nil))[1]
```

```txt
in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
   │
 3 │ ((nil))()
   │ ^^^^^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
   │
 5 │ local _ = ((nil))[1]
   │           ^^^^^^^
```

```diff
@ -2,5 +2,5 @
  -- config: (lint (only syntax:redundant-parens))
  (nil)() ; -- ok
- ((nil))()
+ (nil)()
  local _ = (nil)[1] -- ok
- local _ = ((nil))[1]
+ local _ = (nil)[1]
```

# Redundant parenthesis around calls
```lua
-- config: (lint (only syntax:redundant-parens))
local _, _ = (f()) -- ok
local _ = (f())

f((f())) -- ok
f((f()), 0)

local _ = { (f()) } -- ok
local _ = { (f()), 0 }

local function _() return (f()) end -- ok
local function _() return (f()), 0 end
```

```txt
in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
   │
 3 │ local _ = (f())
   │           ^^^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
   │
 6 │ f((f()), 0)
   │   ^^^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
   │
 9 │ local _ = { (f()), 0 }
   │             ^^^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
    │
 12 │ local function _() return (f()), 0 end
    │                           ^^^^^
```

```diff
@ -2,12 +2,12 @
  -- config: (lint (only syntax:redundant-parens))
  local _, _ = (f()) -- ok
- local _ = (f())
+ local _ = f()

  f((f())) -- ok
- f((f()), 0)
+ f(f(), 0)

  local _ = { (f()) } -- ok
- local _ = { (f()), 0 }
+ local _ = { f(), 0 }

  local function _() return (f()) end -- ok
- local function _() return (f()), 0 end
+ local function _() return f(), 0 end
```

# Redundant parenthesis around dots
```lua
-- config: (lint (only syntax:redundant-parens))
local _, _ = (...) -- ok
local _ = (...)

f((...)) -- ok
f((...), 0)

local _ = { (...) } -- ok
local _ = { (...), 0 }

local function _(...) return (...) end -- ok
local function _(...) return (...), 0 end
-- TODO: local function _(...) return(x)end
```

```txt
in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
   │
 3 │ local _ = (...)
   │           ^^^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
   │
 6 │ f((...), 0)
   │   ^^^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
   │
 9 │ local _ = { (...), 0 }
   │             ^^^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
    │
 12 │ local function _(...) return (...), 0 end
    │                              ^^^^^
```

```diff
@ -2,13 +2,13 @
  -- config: (lint (only syntax:redundant-parens))
  local _, _ = (...) -- ok
- local _ = (...)
+ local _ = ...

  f((...)) -- ok
- f((...), 0)
+ f(..., 0)

  local _ = { (...) } -- ok
- local _ = { (...), 0 }
+ local _ = { ..., 0 }

  local function _(...) return (...) end -- ok
- local function _(...) return (...), 0 end
+ local function _(...) return ..., 0 end
  -- TODO: local function _(...) return(x)end
```

# Operators
```lua
-- config: (lint (only syntax:redundant-parens))
local _ = (2 + 2)
local _ = (2 + 2)() -- ok
local _ = (2 + 2)[1] -- ok

local _ = (#2)
local _ = (#2)() -- ok
local _ = (#2)[1] -- ok

local _ = (2 + 2) * 2 -- ok
local _ = 2 + (2 * 2)
local _ = 2 + (2 + 2) -- ok
local _ = 2 * (2 + 2) + 1 -- ok
local _ = (2 + 2) + 2

local _ = not(x)
local _ = (x)and(x)
```

```txt
in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
   │
 2 │ local _ = (2 + 2)
   │           ^^^^^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
   │
 6 │ local _ = (#2)
   │           ^^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
    │
 11 │ local _ = 2 + (2 * 2)
    │               ^^^^^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
    │
 14 │ local _ = (2 + 2) + 2
    │           ^^^^^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
    │
 16 │ local _ = not(x)
    │              ^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
    │
 17 │ local _ = (x)and(x)
    │           ^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
    │
 17 │ local _ = (x)and(x)
    │                 ^^^
```

```diff
@ -1,17 +1,17 @
  -- config: (lint (only syntax:redundant-parens))
- local _ = (2 + 2)
+ local _ = 2 + 2
  local _ = (2 + 2)() -- ok
  local _ = (2 + 2)[1] -- ok

- local _ = (#2)
+ local _ = #2
  local _ = (#2)() -- ok
  local _ = (#2)[1] -- ok

  local _ = (2 + 2) * 2 -- ok
- local _ = 2 + (2 * 2)
+ local _ = 2 + 2 * 2
  local _ = 2 + (2 + 2) -- ok
  local _ = 2 * (2 + 2) + 1 -- ok
- local _ = (2 + 2) + 2
+ local _ = 2 + 2 + 2

- local _ = not(x)
- local _ = (x)and(x)
+ local _ = not x
+ local _ = x and x
```

# Allow clarifying
```lua
-- config: (lint (only syntax:redundant-parens) (allow-clarifying-parens true))

local _ = (2 + 2)
local _ = (2 + 2)() -- ok
local _ = (2 + 2)[1] -- ok

local _ = (2 + 2) * 2 -- ok
local _ = 2 + (2 * 2) -- ok
local _ = 2 + (2 + 2) -- ok
local _ = (2 + 2) + 2 -- ok

local _ = ((2 + 2)) * 2
local _ = 2 + ((2 * 2))
local _ = 2 + ((2 + 2))
local _ = ((2 + 2)) + 2
```

```txt
in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
   │
 3 │ local _ = (2 + 2)
   │           ^^^^^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
    │
 12 │ local _ = ((2 + 2)) * 2
    │           ^^^^^^^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
    │
 13 │ local _ = 2 + ((2 * 2))
    │               ^^^^^^^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
    │
 14 │ local _ = 2 + ((2 + 2))
    │               ^^^^^^^^^

in.lua: Unnecessary parenthesis. [syntax:redundant-parens]
    │
 15 │ local _ = ((2 + 2)) + 2
    │           ^^^^^^^^^
```

```diff
@ -2,6 +2,6 @
  -- config: (lint (only syntax:redundant-parens) (allow-clarifying-parens true))

- local _ = (2 + 2)
+ local _ = 2 + 2
  local _ = (2 + 2)() -- ok
  local _ = (2 + 2)[1] -- ok

@ -17,21 +17,21 @
  local _ = 2 + (2 + 2) -- ok
  local _ = (2 + 2) + 2 -- ok

- local _ = ((2 + 2)) * 2
- local _ = 2 + ((2 * 2))
- local _ = 2 + ((2 + 2))
- local _ = ((2 + 2)) + 2
+ local _ = (2 + 2) * 2
+ local _ = 2 + (2 * 2)
+ local _ = 2 + (2 + 2)
+ local _ = (2 + 2) + 2
```
