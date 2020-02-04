-- config: (lint (only doc:type-mismatch doc:kind-mismatch))

--- Marked as type and function
--
-- @param x 2
-- @type Foo
local function f(x) end

--- Marked as literal and function
local x = 0
x = function() end
