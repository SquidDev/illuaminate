-- config: (lint (only doc:detached-comment))
local x
--- This is a detached documentation comment


--- This is attached to something
local x = 0

local x = {
  x = 0, --[[-useful]] --- detached
}

--- Also detached
