-- config: (lint (only (var:pointless-discard)))

local _ = 0
local _, x = 0 -- ok

_, _ = 0
_, a = 0 -- ok

for _ in pairs({}) do end -- ok
for _, _ in pairs({}) do end
for _, v in pairs({}) do end -- ok

local function f(_) end
local function f(_, ...) end -- ok
