-- config: (lint (only (stdlib:string-len)))

local x = string
local y = string.len
local z = x.len

local _ = x.len "foo"
local _ = y "foo"
local _ = z "foo"
