-- config: (lint (only (syntax:malformed-number)))
local _ = {
  2..,
  2...,
  2.2..,
  2e2.2,

  -- ok
  2.3,
  2 .. 0,
  2. .. 0,
  2e3 .. 0,
  0x2 ..0,
}
