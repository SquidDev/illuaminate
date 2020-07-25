local function printf(str, ...)
  print(string.format(str, ...))
end

local h, s = {}, 1
for i = 1, 10 do
  printf("Hello", i * 2)
end
