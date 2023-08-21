See [`lint_trailing_table.ml`](../../src/lint/lint_trailing_table.ml)

# Commas
```lua
-- config: (lint (only format:table-trailing))

local _ = {} -- ok
local _ = { 0 } -- ok
local _ = { -- ok
  0, 1,
}

local _ = {
  0
}

local _ = { 0, }
```

```txt
in.lua: Expected trailing "," on multiline table [format:table-trailing]
    │
 10 │   0
    │   ^

in.lua: Unexpected trailing "," on single line table [format:table-trailing]
    │
 13 │ local _ = { 0, }
    │              ^
```

```diff
@ -9,13 +9,13 @
  }

  local _ = {
-   0
+   0,
  }

- local _ = { 0, }
+ local _ = { 0 }
```

# Semicolons
```lua
-- config: (lint (only format:table-trailing) (table-separator semicolon))

local _ = {
  0
}
```

```txt
in.lua: Expected trailing ";" on multiline table [format:table-trailing]
   │
 4 │   0
   │   ^
```

```diff
@ -3,5 +3,5 @
  -- config: (lint (only format:table-trailing) (table-separator semicolon))

  local _ = {
-   0
+   0;
  }
```
