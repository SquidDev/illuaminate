-- config: (lint (only (syntax:misplaced-dots)))

local function f(..., x) end
local function f(x, ...) end -- ok
