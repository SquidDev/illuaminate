-- config: (lint (only (syntax:invalid-break)))

break

while true do
  break -- ok

  local function f()
    break
  end
end
