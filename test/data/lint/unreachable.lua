-- config: (lint (only (control:unreachable)))

local f = ...

local function _()
  do return end

  print()
end

local function _()
  if f() then
    return 1
  else
    return 2
  end

  if f() then
    return 1
  else
    return 2
  end

  return
end

local function _()
  while true do end

  return
end

local function _()
  while true do
    if f() then break end
  end

  return -- ok
end

local function _()
  repeat
    return
  until f()
end


local function _()
  while true do
    do break end

    print()
  end
end

local function _()
  if true then
  elseif f() then
    print() -- ok
  elseif false then
    print()
  end

end

do return end
print()
