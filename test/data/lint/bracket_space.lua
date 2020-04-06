-- config: (lint (only format:bracket-space))

print(1 ) print( 1) local _ = print(1 )
local _ = ( 1), (1 )
local _ = { 1}, {1 }
local _ = { [1 ] = true }, { [ 1] = true }
local _ = x[1 ], x[ 1]

local function f( x) end
local function f(x ) end

function f( x) end
function f(x ) end

local f = function( x) end
local f = function(x ) end
