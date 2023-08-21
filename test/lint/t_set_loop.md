See [`lint_set_loop.ml`](../../src/lint/lint_set_loop.ml)

```lua
-- config: (lint (only var:set-loop))
for x = 0, 10 do
  x = x + 1
end

for k in pairs(_G) do
  k = 0
end
```

```txt
in.lua: Mutating loop variable "x". [var:set-loop]
   │
 3 │   x = x + 1
   │   ^

in.lua: Mutating loop variable "k". [var:set-loop]
   │
 7 │   k = 0
   │   ^
```
