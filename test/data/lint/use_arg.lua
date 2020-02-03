-- config: (lint (only var:use-arg))

local function f(...)
  print(arg)
  arg = nil
  arg[0] = nil

  local arg = 0
  print(arg) -- ok
end
