See [`lint_malformed_number.ml`](../../src/lint/lint_malformed_number.ml)

```lua
-- config: (lint (only syntax:malformed-number))
local _ = {
  2..,
  2...,
  2.2..,
  2e2.2,

  -- ok
  2.3,
  2 .. 0,
  2. .. 0,
  2e3 .. 0,
  0x2 ..0,
}
```

```txt
in.lua: Malformed number [syntax:malformed-number]
   │
 3 │   2..,
   │   ^^^

in.lua: Malformed number [syntax:malformed-number]
   │
 4 │   2...,
   │   ^^^^

in.lua: Malformed number [syntax:malformed-number]
   │
 5 │   2.2..,
   │   ^^^^^

in.lua: Malformed number [syntax:malformed-number]
   │
 6 │   2e2.2,
   │   ^^^^^
```
