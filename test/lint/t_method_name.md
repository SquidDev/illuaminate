See [`lint_method_name.ml`](../../src/lint/lint_method_name.ml)

```lua
-- config: (lint (only syntax:method-name))

function x:y:z() end
function x:y.z() end
function x.y:z() end --ok
```

```txt
in.lua: Method names cannot appear at this position. [syntax:method-name]
   │
 3 │ function x:y:z() end
   │            ^

in.lua: Method names cannot appear at this position. [syntax:method-name]
   │
 4 │ function x:y.z() end
   │            ^
```
