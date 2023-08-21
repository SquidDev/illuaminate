See [`lint_string_lib.ml`](../../src/lint/lint_string_lib.ml)

# `string.format` specifiers
```lua
-- config: (lint (only stdlib:string-format))

local _ = string.format("%s %.2f % +s") -- ok
local _ = string.format("%?")

local _ = string.format("%2%")
```

```txt
in.lua: Format string takes 3 parameters, but only given 0 arguments. [stdlib:string-format]
   │
 3 │ local _ = string.format("%s %.2f % +s") -- ok
   │           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

in.lua: Unknown specifier '%?' [stdlib:string-format]
   │
 4 │ local _ = string.format("%?")
   │                         ^^^^

in.lua: Redundant flags to format string '%' [stdlib:string-format]
   │
 6 │ local _ = string.format("%2%")
   │                         ^^^^^
```

# `string.format` arity
```lua
-- config: (lint (only stdlib:string-format))

local _ = string.format("%s %s", "a", "b") -- ok
local _ = ("%s %s"):format("a", "b") -- ok

local _ = string.format("%s %s", "a")
local _ = string.format("%s %s", "a", ...) -- ok
local _ = string.format("%s %s", "a", "b", ...)
local _ = string.format("%s %s", "a", "b", "c")
local _ = string.format("%s %s", "a", "b", "c", ...)

local _ = ("%s %s"):format("a")

local _ = string.format("%%")
```

```txt
in.lua: Format string takes 2 parameters, but only given 1 argument. [stdlib:string-format]
   │
 6 │ local _ = string.format("%s %s", "a")
   │           ^^^^^^^^^^^^^^^^^^^^^^^^^^^

in.lua: Format string takes 2 parameters, but given an extra 1 argument. [stdlib:string-format]
   │
 8 │ local _ = string.format("%s %s", "a", "b", ...)
   │                                            ^^^

in.lua: Format string takes 2 parameters, but given an extra 1 argument. [stdlib:string-format]
   │
 9 │ local _ = string.format("%s %s", "a", "b", "c")
   │                                            ^^^

in.lua: Format string takes 2 parameters, but given an extra 2 arguments. [stdlib:string-format]
    │
 10 │ local _ = string.format("%s %s", "a", "b", "c", ...)
    │                                            ^^^^^^^^

in.lua: Format string takes 2 parameters, but only given 1 argument. [stdlib:string-format]
    │
 12 │ local _ = ("%s %s"):format("a")
    │           ^^^^^^^^^^^^^^^^^^^^^
```
