-- config: (lint (only syntax:redundant-parens))

-- Redundant parenthesis around references
do
  (f)((f), {
      (f)
  })
end

-- Redundant parenthesis around literals
do
  (nil)() ; -- ok
  ((nil))()
  local _ = (nil)[1] -- ok
  local _ = ((nil))[1]
end


-- Redundant parenthesis around calls
do
  local _, _ = (f()) -- ok
  local _ = (f())

  f((f())) -- ok
  f((f()), 0)

  local _ = { (f()) } -- ok
  local _ = { (f()), 0 }

  local function _() return (f()) end -- ok
  local function _() return (f()), 0 end
end

-- Redundant parenthesis around dots
do
  local _, _ = (...) -- ok
  local _ = (...)

  f((...)) -- ok
  f((...), 0)

  local _ = { (...) } -- ok
  local _ = { (...), 0 }

  local function _(...) return (...) end -- ok
  local function _(...) return (...), 0 end
  -- TODO: local function _(...) return(x)end
end

-- Operators
do
  local _ = (2 + 2)
  local _ = (2 + 2)() -- ok
  local _ = (2 + 2)[1] -- ok

  local _ = (#2)
  local _ = (#2)() -- ok
  local _ = (#2)[1] -- ok

  local _ = (2 + 2) * 2 -- ok
  local _ = 2 + (2 * 2)
  local _ = 2 + (2 + 2) -- ok
  local _ = 2 * (2 + 2) + 1 -- ok
  local _ = (2 + 2) + 2

  local _ = not(x)
  local _ = (x)and(x)
end
