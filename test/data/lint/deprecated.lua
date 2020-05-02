--- @deprecated
local x

--- @deprecated Use something else instead
local y

x = {} -- Assignments to deprecated members are OK.

-- Usages are not.
x.a = 1
print(x, y)
