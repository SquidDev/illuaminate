See [`lint_malformed_example.ml`](../../src/lint/lint_malformed_example.ml)

# Markdown code blocks

```lua
--- config: (lint (only doc:malformed-example))
-- Checks malformed examples and other code blocks
-- @module Test

--- ```lua
-- Lua fenced code block
-- ```
--
--     Code block, but not marked as Lua
--
-- ```
-- Plain text code block
-- ```
function description_fail() end
```

```txt
in.lua: Cannot parse example [doc:malformed-example]
   │
 6 │ -- Lua fenced code block
   │        ^^^^^^
Unexpected identifier: Expected eof after an expression.
```

# `@usage comments`

```lua
--- config: (lint (only doc:malformed-example))
-- Checks malformed examples and other code blocks
-- @module Test

--- @usage Single-line usage
-- @usage Multiline-code block usage
-- ```lua
-- Fenced in example
-- ```
--
--     Indented in example
--
-- Inline `code should be fine`.
function usage() end
```

```txt
in.lua: Cannot parse example [doc:malformed-example]
   │
 5 │ --- @usage Single-line usage
   │                        ^^^^^
Unexpected identifier: Expected eof after an expression.

in.lua: Cannot parse example [doc:malformed-example]
   │
 8 │ -- Fenced in example
   │           ^^
Unexpected `in`: This is unexpected after a simple expression.

in.lua: Cannot parse example [doc:malformed-example]
    │
 11 │ --     Indented in example
    │                 ^^
Unexpected `in`: This is unexpected after a simple expression.
```

# Valid examples have no errors
```lua
--- config: (lint (only doc:malformed-example))
-- Checks malformed examples and other code blocks
-- @module Test

-- ```lua
-- return 0 -- statement
-- ```
-- ```lua
-- 0 -- expr
-- ```
--- ```lua
-- local more_complex
-- print(example)
-- ```
function description_pass() end
```

```txt
No errors
```
