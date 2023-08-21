See [`lint_pcall_eta.ml`](../../src/lint/lint_pcall_eta.ml)

```lua
-- config: (lint (only stdlib:pcall-eta))

pcall(function() f(1, 2, 3) end)
pcall(function() return f(1, 2, 3) end)

pcall(function() return f(f()) end) -- skip: side effect in args
pcall(function(x) return f(x) end) -- skip: function arguments
pcall(function() f(1, 2, 3) end, 1) -- skip: applying additional arguments
pcall(function() f() g() end) -- skip: complex block

-- Warns but doesn't fix.
-- TODO: Handle this correctly
pcall(function()
  return f(
      1
  )
end)

local ok = pcall(function() f(1, 2, 3) end)
```

```txt
in.lua: Prefer passing function arguments to pcall, rather than using a closure. [stdlib:pcall-eta]
   │
 3 │ pcall(function() f(1, 2, 3) end)
   │ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

in.lua: Prefer passing function arguments to pcall, rather than using a closure. [stdlib:pcall-eta]
   │
 4 │ pcall(function() return f(1, 2, 3) end)
   │ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

in.lua: Prefer passing function arguments to pcall, rather than using a closure. [stdlib:pcall-eta]
    │
 13 │ pcall(function()
    │ ^^^^^^^^^^^^^^^^

in.lua: Prefer passing function arguments to pcall, rather than using a closure. [stdlib:pcall-eta]
    │
 19 │ local ok = pcall(function() f(1, 2, 3) end)
    │            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

```diff
@ -2,7 +2,7 @
  -- config: (lint (only stdlib:pcall-eta))

- pcall(function() f(1, 2, 3) end)
- pcall(function() return f(1, 2, 3) end)
+ pcall(f, 1, 2, 3)
+ pcall(f, 1, 2, 3)

  pcall(function() return f(f()) end) -- skip: side effect in args
  pcall(function(x) return f(x) end) -- skip: function arguments
@ -24,25 +24,25 @
    )
  end)

- local ok = pcall(function() f(1, 2, 3) end)
+ local ok = pcall(f, 1, 2, 3)
```
