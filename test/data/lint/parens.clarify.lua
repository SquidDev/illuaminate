-- config: (lint (only (syntax:redundant-parens)) (allow-clarifying-parens true))

local _ = (2 + 2)
local _ = (2 + 2)() -- ok
local _ = (2 + 2)[1] -- ok

local _ = (2 + 2) * 2 -- ok
local _ = 2 + (2 * 2) -- ok
local _ = 2 + (2 + 2) -- ok
local _ = (2 + 2) + 2 -- ok

local _ = ((2 + 2)) * 2
local _ = 2 + ((2 * 2))
local _ = 2 + ((2 + 2))
local _ = ((2 + 2)) + 2
