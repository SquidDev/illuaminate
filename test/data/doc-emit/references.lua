--- @module references

local MyType = {} --- @type MyType

function MyType:meth() end

return {
  my_term = {},

  --- @see unknown
  unknown_1 = function() end,

  --- See @{unknown} and @{unknown|custom label}.
  unknown_2 = function() end,

  --- @see my_term
  -- @see references.my_term
  term_1 = function() end,

  --- See @{my_term}, @{references.my_term} and @{my_term|custom label}.
  term_2 = function() end,

  --- @see my_term
  -- @see references.my_term
  -- @see library!references.my_term
  type_1 = function() end,

  --- See @{MyType} and @{references.MyType}
  type_2 = function() end,

  --- See @{MyType}, @{references.MyType} and @{library!references.MyType}
  -- @treturn MyType
  type_3 = function() end,

  --- See @{MyType:meth} and @{MyType.meth}
  method_1 = function() end,

  --- @see string.match
  -- @see string
  -- @see number
  builtin_1 = function () end,

  --- @{string.match}, @{string} and @{number}
  builtin_2 = function () end,

  --- @{references}
  module_1 = function () end,
}
