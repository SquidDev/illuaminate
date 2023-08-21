See [`lint_string_len.ml`](../../src/lint/lint_string_len.ml)

```lua
-- config: (lint (only stdlib:string-len))

local _ = string.len "foo"
local _ = string.len("foo") -- Basic application
local _ = string.len("foo" .. "bar") -- Needs parens
local _ = string.len("foo", "bar") -- Warns, but no change

-- Shouldn't insert double spaces.
local _ = string.len( "foo" )
local _ = string.len( "foo" ) + 2

-- All of these fine
local _ = #"foo"
local _ = string.lens("foo")
local _ = strings.len("foo")
```

```txt
in.lua: Prefer `#` over string.len [stdlib:string-len]
   │
 3 │ local _ = string.len "foo"
   │           ^^^^^^^^^^^^^^^^

in.lua: Prefer `#` over string.len [stdlib:string-len]
   │
 4 │ local _ = string.len("foo") -- Basic application
   │           ^^^^^^^^^^^^^^^^^

in.lua: Prefer `#` over string.len [stdlib:string-len]
   │
 5 │ local _ = string.len("foo" .. "bar") -- Needs parens
   │           ^^^^^^^^^^^^^^^^^^^^^^^^^^

in.lua: Prefer `#` over string.len [stdlib:string-len]
   │
 6 │ local _ = string.len("foo", "bar") -- Warns, but no change
   │           ^^^^^^^^^^^^^^^^^^^^^^^^

in.lua: Prefer `#` over string.len [stdlib:string-len]
   │
 9 │ local _ = string.len( "foo" )
   │           ^^^^^^^^^^^^^^^^^^^

in.lua: Prefer `#` over string.len [stdlib:string-len]
    │
 10 │ local _ = string.len( "foo" ) + 2
    │           ^^^^^^^^^^^^^^^^^^^
```

```diff
@ -2,13 +2,13 @
  -- config: (lint (only stdlib:string-len))

- local _ = string.len "foo"
- local _ = string.len("foo") -- Basic application
- local _ = string.len("foo" .. "bar") -- Needs parens
+ local _ = #"foo"
+ local _ = #"foo" -- Basic application
+ local _ = #("foo" .. "bar") -- Needs parens
  local _ = string.len("foo", "bar") -- Warns, but no change

  -- Shouldn't insert double spaces.
- local _ = string.len( "foo" )
- local _ = string.len( "foo" ) + 2
+ local _ = #"foo"
+ local _ = #"foo" + 2

  -- All of these fine
  local _ = #"foo"
```

# Through indirect accesses to `string.len`
```lua
-- config: (lint (only stdlib:string-len))

local x = string
local y = string.len
local z = x.len

local _ = x.len "foo"
local _ = y "foo"
local _ = z "foo"
```

```txt
in.lua: Prefer `#` over string.len [stdlib:string-len]
   │
 7 │ local _ = x.len "foo"
   │           ^^^^^^^^^^^

in.lua: Prefer `#` over string.len [stdlib:string-len]
   │
 8 │ local _ = y "foo"
   │           ^^^^^^^

in.lua: Prefer `#` over string.len [stdlib:string-len]
   │
 9 │ local _ = z "foo"
   │           ^^^^^^^
```

```diff
@ -6,9 +6,9 @
  local y = string.len
  local z = x.len

- local _ = x.len "foo"
- local _ = y "foo"
- local _ = z "foo"
+ local _ = #"foo"
+ local _ = #"foo"
+ local _ = #"foo"
```
