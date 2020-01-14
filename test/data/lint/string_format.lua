-- config: (lint (only (stdlib:string-format)))

local _ = string.format("%s %.2f % +s") -- ok
local _ = string.format("%?")

local _ = string.format("%2%")
