-- config: (lint (only var:unknown-global) (globals :lua5.3 foo))

-- foo is a custom global so is ok
print(foo)

-- getfenv is not present in Lua 5.3, so produces a warning.
print(getfenv)
