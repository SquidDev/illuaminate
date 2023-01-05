--- config: (lint (only doc:unresolved-reference))
-- Foo
-- @module x

--- @type Foo
local Foo = {}

return {
  --- A small test
  --
  -- See a @{UnknownDescription}
  --
  -- @tparam UnknownArgType f A description
  -- @treturn UnknownRetType Description
  -- @see UnknownSee
  -- @see Foo This and @{has_docs} should be resolved
  -- @usage See @{UnknownExample}
  -- ```lua
  -- Foo
  -- ```
  has_docs = function(f) end,
}
