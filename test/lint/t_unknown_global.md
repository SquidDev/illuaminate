See [`lint_unknown_global.ml`](../../src/lint/lint_unknown_global.ml)

# With the default config
```lua
-- config: (lint (only var:unknown-global))

-- `a` is local and b is a bound module within this scope
local a
b = 0
print(a, b, c, c_module)

-- getfenv is present in the default (max) global list
print(getfenv)

function a:foo() return self end -- ok
print(self) -- not ok
```

```txt
in.lua: Using unknown global "c" [var:unknown-global]
   │
 6 │ print(a, b, c, c_module)
   │             ^

in.lua: Using unknown global "self" [var:unknown-global]
    │
 12 │ print(self) -- not ok
    │       ^^^^
```

# With a custom global
```lua
-- config: (lint (only var:unknown-global) (globals :lua5.3 foo))

-- foo is a custom global so is ok
print(foo)

-- getfenv is not present in Lua 5.3, so produces a warning.
print(getfenv)
```

```txt
in.lua: Using unknown global "getfenv" [var:unknown-global]
   │
 7 │ print(getfenv)
   │       ^^^^^^^
```
