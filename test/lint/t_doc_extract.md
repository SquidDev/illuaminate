See [`lint_doc_extract.ml`](../../src/lint/lint_doc_extract.ml)

```lua
-- config: (lint (only doc:type-mismatch doc:kind-mismatch))

--- Marked as type and function
--
-- @param x 2
-- @type Foo
local function f(x) end

--- Marked as literal and function
local x = 0
x = function() end
```

```txt
in.lua: Documentation comment cannot have both @param/@return and @type [doc:kind-mismatch]
   │
 3 │ --- Marked as type and function
   │ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

in.lua: Conflicting definitions, cannot merge `? = 0` and `function()` [doc:kind-mismatch]
    │
 10 │ local x = 0
    │           ^
```
