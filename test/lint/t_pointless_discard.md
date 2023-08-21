See [`lint_pointless_discard.ml`](../../src/lint/lint_pointless_discard.ml)

```lua
-- config: (lint (only var:pointless-discard))

local _ = 0
local _, x = 0 -- ok
local a, _ = f(), 0

_, _ = 0
_, a = 0 -- ok
a, _ = f(), 0

-- TODO: This converts into `f()g()h()', which is correct but terribly ugly. The
-- linter driver needs to correctly handle this.
_ = { [f()] = g(), h(), 2 .. 2 }

for _ in pairs({}) do end -- ok
for _, _ in pairs({}) do end
for _, v in pairs({}) do end -- ok
for k, _ in pairs({}) do end

local function f(_) end
local function f(_, a) end -- ok
local function f(_, ...) end -- ok

local function _() end
function _() end
```

```txt
in.lua: Pointless discard variable `_`. [var:pointless-discard]
   │
 3 │ local _ = 0
   │       ^

in.lua: Pointless discard variable `_`. [var:pointless-discard]
   │
 5 │ local a, _ = f(), 0
   │          ^

in.lua: Pointless discard variable `_`. [var:pointless-discard]
   │
 7 │ _, _ = 0
   │    ^

in.lua: Pointless discard variable `_`. [var:pointless-discard]
   │
 9 │ a, _ = f(), 0
   │    ^

in.lua: Pointless discard variable `_`. [var:pointless-discard]
    │
 13 │ _ = { [f()] = g(), h(), 2 .. 2 }
    │ ^

in.lua: Pointless discard variable `_`. [var:pointless-discard]
    │
 16 │ for _, _ in pairs({}) do end
    │        ^

in.lua: Pointless discard variable `_`. [var:pointless-discard]
    │
 18 │ for k, _ in pairs({}) do end
    │        ^

in.lua: Pointless discard variable `_`. [var:pointless-discard]
    │
 20 │ local function f(_) end
    │                  ^

in.lua: Pointless discard variable `_`. [var:pointless-discard]
    │
 24 │ local function _() end
    │                ^

in.lua: Pointless discard variable `_`. [var:pointless-discard]
    │
 25 │ function _() end
    │          ^
```

```diff
@ -1,15 +1,26 @
- -- config: (lint (only var:pointless-discard))
-
- local _ = 0
  local _, x = 0 -- ok
- local a, _ = f(), 0
-
- _, _ = 0
+ local a = f(), 0
  _, a = 0 -- ok
- a, _ = f(), 0
-
- -- TODO: This converts into `f()g()h()', which is correct but terribly ugly. The
- -- linter driver needs to correctly handle this.
- _ = { [f()] = g(), h(), 2 .. 2 }
-
+ a = f(), 0
+ f()g()h()
  for _ in pairs({}) do end -- ok
- for _, _ in pairs({}) do end
+ for _ in pairs({}) do end
  for _, v in pairs({}) do end -- ok
- for k, _ in pairs({}) do end
+ for k in pairs({}) do end

- local function f(_) end
+ local function f() end
  local function f(_, a) end -- ok
  local function f(_, ...) end -- ok

- local function _() end
- function _() end
```
