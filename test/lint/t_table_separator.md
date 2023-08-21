See [`lint_table_separator.ml`](../../src/lint/lint_table_separator.ml)

# Require commas

```lua
-- config: (lint (only format:table-separator))

local _ = { a, b; c, d; }
```

```txt
in.lua: Table fields should be separated by a comma. [format:table-separator]
   │
 3 │ local _ = { a, b; c, d; }
   │                 ^

in.lua: Table fields should be separated by a comma. [format:table-separator]
   │
 3 │ local _ = { a, b; c, d; }
   │                       ^
```

```diff
@ -2,3 +2,3 @
  -- config: (lint (only format:table-separator))

- local _ = { a, b; c, d; }
+ local _ = { a, b, c, d, }
```

# Require semicolons

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
