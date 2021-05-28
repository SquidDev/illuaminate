--- @module foo

return setmetatable({
    a = 1 --- Exposed through the metatable
}, {})
