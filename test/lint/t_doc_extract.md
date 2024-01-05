See [`lint_doc_extract.ml`](../../src/lint/lint_doc_extract.ml)

```lua
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
in.lua: Documentation comment cannot have both @param/@return and @type [doc:extract]
   │
 1 │ --- Marked as type and function
   │ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

in.lua: Conflicting definitions, cannot merge `? = 0` and `function()` [doc:extract]
   │
 8 │ local x = 0
   │           ^
```
