-- config: (lint (only (stdlib:string-format)))

local _ = string.format("%s %s", "a", "b") -- ok
local _ = ("%s %s"):format("a", "b") -- ok

local _ = string.format("%s %s", "a")
local _ = string.format("%s %s", "a", ...) -- ok
local _ = string.format("%s %s", "a", "b", ...)
local _ = string.format("%s %s", "a", "b", "c")
local _ = string.format("%s %s", "a", "b", "c", ...)

local _ = ("%s %s"):format("a")

local _ = string.format("%%")
