See [`lint_detached_comment.ml`](../../src/lint/lint_detached_comment.ml)

```lua
-- config: (lint (only doc:detached-comment))
local x
--- This is a detached documentation comment


--- This is attached to something
local x = 0

local x = {
  x = 0, --[[-useful]] --- detached
}

--- Also detached
```

```txt
in.lua: Detached doc comment, this will not be processed [doc:detached-comment]
   │
 3 │ --- This is a detached documentation comment
   │ ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

in.lua: Detached doc comment, this will not be processed [doc:detached-comment]
    │
 10 │   x = 0, --[[-useful]] --- detached
    │                        ^^^^^^^^^^^^

in.lua: Detached doc comment, this will not be processed [doc:detached-comment]
    │
 13 │ --- Also detached
    │ ^^^^^^^^^^^^^^^^^
```
