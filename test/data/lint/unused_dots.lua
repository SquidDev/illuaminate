-- config: (lint (only var:unused))

local function f(x, ...) return x end
local function g(...) end

local function h(...) return ... end -- ok
local function i(...) return arg end -- ok

return { f, g, h, i }
