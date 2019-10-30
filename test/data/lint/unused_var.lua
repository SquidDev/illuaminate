-- config: (lint (only (var:unused)))
local x, y, z, _ = 0, 0, 0
print(x)

y = 0
z[0] = 0

-- FIXME: y = 0 should not warn for unused var (unused val? Sure)
-- TODO: Is z bound or not?
