-- config: (lint (only (var:pointless-discard)))

local _ = 0
local _, x = 0 -- ok
local a, _ = f(), 0

_, _ = 0
_, a = 0 -- ok
a, _ = f(), 0

-- TODO: This converts into `f()g()h()', which is correct but terribly ugly. The
-- linter driver needs to correctly handle this.
_ = { [f()] = g(), h(), 2 .. 2 }

for _ in pairs({}) do end -- ok
for _, _ in pairs({}) do end
for _, v in pairs({}) do end -- ok
for k, _ in pairs({}) do end

local function f(_) end
local function f(_, a) end -- ok
local function f(_, ...) end -- ok

local function _() end
function _() end
