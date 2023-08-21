See [`lint_pointless_semicolon.ml`](../../src/lint/lint_pointless_semicolon.ml)

```lua
-- config: (lint (only syntax:redundant-semicolon))
local function f() end

do ; end

-- FIXME: We need to preserve trivia!
do local x; end

do ; local x end

do f(); (f)() end

do f(); f() end
```

```txt
in.lua: Redundant semicolon. [syntax:redundant-semicolon]
   │
 4 │ do ; end
   │    ^

in.lua: Redundant semicolon. [syntax:redundant-semicolon]
   │
 7 │ do local x; end
   │           ^

in.lua: Redundant semicolon. [syntax:redundant-semicolon]
   │
 9 │ do ; local x end
   │    ^

in.lua: Redundant semicolon. [syntax:redundant-semicolon]
    │
 13 │ do f(); f() end
    │       ^
```

```diff
@ -3,13 +3,13 @
  -- config: (lint (only syntax:redundant-semicolon))
  local function f() end

- do ; end
+ do end

  -- FIXME: We need to preserve trivia!
- do local x; end
+ do local xend

- do ; local x end
+ do local x end

  do f(); (f)() end

- do f(); f() end
+ do f()f() end
```
