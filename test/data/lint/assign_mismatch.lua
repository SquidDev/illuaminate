-- config: (lint (only var:unbalanced-assign))

local x, y = 0 -- ok
local x, y = 0, 0, 0

x, y = 0
x, y = 0, 0, 0
x, y = ...
