--- @module types

--- @type Foo
local Foo = {}

return {
  --- A basic exported _value_.
  no_clue = {},

  --- Type syntax
  --
  -- @tparam string a
  -- @tparam string|number b

  ty_1 = function(a, b) end,

  --- Function type syntax
  -- @tparam function():number|string a
  -- @tparam function(string) b
  -- @tparam function(f: string) c
  -- @tparam function([string]) d
  -- @tparam function([f: string]) e
  -- @tparam function(x:int[, f: string]) f
  -- @tparam function(x:int, f: string) g
  ty_2 = function(...) end,

  --- Table syntax
  --
  -- @tparam { foo = number } a
  -- @tparam { foo? = number } b
  -- @tparam { [string] = number } c
  ty_3 = function(a, b, c) end,

  --- Type references
  --
  -- @tparam no_clue a
  -- @tparam Foo b
  ref = function(a, b) end,

  --- Optional arguments
  --
  -- @tparam[opt] string a
  opt = function(a) end,
}
