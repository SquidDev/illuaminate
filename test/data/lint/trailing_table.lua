-- config: (lint (only format:table-trailing))

local _ = {} -- ok
local _ = { 0 } -- ok
local _ = { -- ok
  0, 1,
}

local _ = {
  0
}

local _ = { 0, }
