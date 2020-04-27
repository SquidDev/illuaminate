-- config: (lint (only var:unresolved-member) (dynamic-modules a_module))

local a_module, b_module = require("a_module"), require("b_module")
print(a_module.unknown)
print(b_module.unknown)
