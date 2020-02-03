-- config: (lint (only stdlib:string-len))

local _ = string.len "foo"
local _ = string.len("foo") -- Basic application
local _ = string.len("foo" .. "bar") -- Needs parens
local _ = string.len("foo", "bar") -- Warns, but no change

-- Shouldn't insert double spaces.
local _ = string.len( "foo" )
local _ = string.len( "foo" ) + 2

-- All of these fine
local _ = #"foo"
local _ = string.lens("foo")
local _ = strings.len("foo")
