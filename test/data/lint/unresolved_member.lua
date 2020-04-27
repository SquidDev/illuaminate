-- config: (lint (only var:unresolved-member))

--- This is a documented term
local tbl_1 = { x = 1 }

-- This is not documented
local tbl_2 = { x = 1 }

--- This is also documented
local function not_tbl() end

print(tbl_1.x, tbl_1.y)
print(tbl_2.x, tbl_2.y) -- Doesn't warn as not documented
print(not_tbl.x) -- Doesn't warn as not table


for i = 1, (function()
  local tbl_3 = { x = 1 } --- Documented
  print(tbl_3.y)
end)() do end

do
  local tbl_4 = { x = 1 } --- Documented
  print(tbl_4.y)
end

local a_module = require("a_module")
print(a_module.a) --- Documented
print(a_module.unknown) --- Undocumented
