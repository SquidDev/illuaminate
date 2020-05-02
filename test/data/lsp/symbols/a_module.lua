--- A basic module
-- @module a_module

--- @type Foo
local Foo = {}
function Foo:a() end
function Foo.b() end
Foo.c = 123

return {
  a = function() end, --- @deprecated
  b = 123,
  c = (function() end)(),
}
