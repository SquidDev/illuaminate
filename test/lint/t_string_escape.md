See [`lint_string_escape.ml`](../../src/lint/lint_string_escape.ml)

```lua
-- config: (lint (only syntax:string-escape))
local _ = {
  "a",
  "a\nb",
  "a\0b",
  "a\x00b",
  "a\(b",
  "\"\'",

  'a',
  'a\nb',
  'a\0b',
  'a\x00b',
  'a\(b',

  '\"\'',

  "\u{1F991}",
}
```

```txt
in.lua: Unknown escape character '\('. [syntax:string-escape]
   │
 7 │   "a\(b",
   │     ^^

in.lua: Unknown escape character '\('. [syntax:string-escape]
    │
 14 │   'a\(b',
    │     ^^
```
