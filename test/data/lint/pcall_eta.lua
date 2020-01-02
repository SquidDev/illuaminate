-- config: (lint (only (stdlib:pcall-eta)))

pcall(function() f(1, 2, 3) end)
pcall(function() return f(1, 2, 3) end)

pcall(function() return f(f()) end) -- skip: side effect in args
pcall(function(x) return f(x) end) -- skip: function arguments
pcall(function() f(1, 2, 3) end, 1) -- skip: applying additional arguments
pcall(function() f() g() end) -- skip: complex block

-- Warns but doesn't fix.
-- TODO: Handle this correctly
pcall(function()
  return f(
      1
  )
end)

local ok = pcall(function() f(1, 2, 3) end)
