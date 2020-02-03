-- config: (lint (only control:for-num))

local t, x = ...

-- Counting up
for _ = 1, 10 do end -- ok
for _ = 1, 10, 1 do end -- ok
for _ = 1, 10, -1 do end

-- Counting nowhere
for _ = 1, 10, 0 do end

-- Counting down
for _ = 1, -10 do end
for _ = 1, -10, -1 do end -- ok

-- Counting down on tables
for _ = #t, 1, -1 do end -- ok
for _ = #t, 1, x do end -- ok
for _ = #t, 1 do end

-- No change
for _ = 1, 1 do end
for _ = 1 + 4, 5 do end
