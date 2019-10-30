-- config: (lint (only (var:set-loop)))
for x = 0, 10 do
  x = x + 1
end

for k in pairs(_G) do
  k = 0
end
