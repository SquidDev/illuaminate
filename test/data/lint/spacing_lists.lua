-- config: (lint (only (format:op-space)))

print(0,1, 2)
print { x=0, 0,1, 2 }

local _ = { x=0 }
local _ = { x= 0 }
local _ = { x =0 }
local _ = { x = 0 } -- ok
