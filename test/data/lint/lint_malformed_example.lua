--- config: (lint (only doc:malformed-example))
-- Checks malformed examples and other code blocks
-- @module Test


local x = {}

--- Descriptions which work
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
function x.description_pass() end

--- Descriptions which don't work
-- ```lua
-- Lua fenced code block
-- ```
--
--     Code block, but not marked as Lua
--
-- ```
-- Plain text code block
-- ```
function x.description_fail() end

--- @usage Single-line usage
-- @usage Multiline-code block usage
-- ```lua
-- Fenced in example
-- ```
--
--     Indented in example
function x.usage() end

return x
