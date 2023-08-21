See [`lint_empty_block.ml`](../../src/lint/lint_empty_block.ml)

# Empty `do` statements
```lua
-- config: (lint (only syntax:empty-do))
do end

do print() end
```

```txt
in.lua: Empty do statement [syntax:empty-do]
   │
 2 │ do end
   │ ^^^^^^
```

```diff
@ -1,3 +1,5 @
- -- config: (lint (only syntax:empty-do))
- do end

  do print() end
```

# Empty `if` statements
```lua
-- config: (lint (only syntax:empty-if) (allow-empty-if true))
if x then
elseif y then
elseif z then
  print()
else
end
```

```txt
in.lua: Empty else clause [syntax:empty-if]
   │
 6 │ else
   │ ^^^^
```

```diff
@ -5,6 +5,7 @
  elseif y then
  elseif z then
    print()
- else
  end
```

# Empty `if` statements (allowed)
```lua
-- config: (lint (only syntax:empty-if))
if x then
elseif y then
elseif z then
  print()
else
end
```

```txt
in.lua: Empty if clause [syntax:empty-if]
   │
 2 │ if x then
   │ ^^^^^^^^^

in.lua: Empty elseif clause [syntax:empty-if]
   │
 3 │ elseif y then
   │ ^^^^^^^^^^^^^

in.lua: Empty else clause [syntax:empty-if]
   │
 6 │ else
   │ ^^^^
```

```diff
@ -5,6 +5,7 @
  elseif y then
  elseif z then
    print()
- else
  end
```
