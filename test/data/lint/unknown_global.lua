-- config: (lint (only var:unknown-global))

-- `a` is local and b is a bound module within this scope
local a
b = 0
print(a, b, c, c_module)

-- getfenv is present in the default (max) global list
print(getfenv)

function a:foo() return self end -- ok
print(self) -- not ok
