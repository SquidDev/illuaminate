-- config: (lint (only (format:op-space format:separator-space)))

local x,y, z
local x = 0,1, 2
local x=0
local x =0
local x = 0 -- ok

local x -- dubious, but ok
=
0
