-- config: (lint (only (var:set-global)) (allow-toplevel-global true))
a = 0 -- ok
function b() end -- ok
if true then c = 0 end -- ok

local function f()
  d = 0
  e = 0
end

-- We need to work out what we should do in this case. Arguably it's OK as we're
-- mutating an already defined variable. But mutating API globals within a
-- function is rather bad form.
--[[
local function f()
  a = 0
  b = 0
end
]]
