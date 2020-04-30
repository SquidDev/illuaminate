--- config: (lint (only doc:undocumented))
-- Foo
-- @module

--- @type Foo
local Foo = {}

return {
  has_docs = function() end, --- This has documentation

  no_docs = function() end, -- This doesn't

  --- Has description
  no_args = function(a, b) end,
}
