See [`lint_string_quote.ml`](../../src/lint/lint_string_quote.ml)

# Double quotes (i.e. default options)
```lua
-- config: (lint (only format:string-quote))

print("foo", 'foo', [[foo]])
print("'\n\"\'", '"\n\"\'', [[
"']])
```

```txt
in.lua: String should use double quotes ('"') [format:string-quote]
   │
 3 │ print("foo", 'foo', [[foo]])
   │              ^^^^^

in.lua: String should use double quotes ('"') [format:string-quote]
   │
 4 │ print("'\n\"\'", '"\n\"\'', [[
   │                  ^^^^^^^^^
```

```diff
@ -2,5 +2,5 @
  -- config: (lint (only format:string-quote))

- print("foo", 'foo', [[foo]])
- print("'\n\"\'", '"\n\"\'', [[
+ print("foo", "foo", [[foo]])
+ print("'\n\"\'", "\"\n\"\'", [[
  "']])
```

# Single quotes
```lua
-- config: (lint (only format:string-quote) (quote single))

print("foo", 'foo', [[foo]])
print("'\n\"\'", '"\n\"\'', [[
"']])
```

```txt
in.lua: String should use single quotes ('\'') [format:string-quote]
   │
 3 │ print("foo", 'foo', [[foo]])
   │       ^^^^^

in.lua: String should use single quotes ('\'') [format:string-quote]
   │
 4 │ print("'\n\"\'", '"\n\"\'', [[
   │       ^^^^^^^^^
```

```diff
@ -2,5 +2,5 @
  -- config: (lint (only format:string-quote) (quote single))

- print("foo", 'foo', [[foo]])
- print("'\n\"\'", '"\n\"\'', [[
+ print('foo', 'foo', [[foo]])
+ print('\'\n\"\'', '"\n\"\'', [[
  "']])
```
