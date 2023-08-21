See [`lint_string_index.ml`](../../src/lint/lint_string_index.ml)

```lua
-- config: (lint (only syntax:string-index))

x["foo"] = x["foo"]

x["return"] = 0
x["nil"] = 0
```

```txt
in.lua: String index can be replaced by identifier. [syntax:string-index]
   │
 3 │ x["foo"] = x["foo"]
   │ ^^^^^^^^

in.lua: String index can be replaced by identifier. [syntax:string-index]
   │
 3 │ x["foo"] = x["foo"]
   │            ^^^^^^^^
```

```diff
@ -2,6 +2,6 @
  -- config: (lint (only syntax:string-index))

- x["foo"] = x["foo"]
+ x.foo = x.foo

  x["return"] = 0
  x["nil"] = 0
```
