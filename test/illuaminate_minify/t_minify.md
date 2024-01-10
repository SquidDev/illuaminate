```lua
local function printf(str, ...)
  print(string.format(str, ...))
end

local h, s = {}, 1
for i = 1, 10 do
  printf("Hello", i * 2)
end
```

```txt
local function e(t,...)print(string.format(t,...))end local a,o={},1 for i=1,10
do
e("Hello",i*2)end
```


# Literals
Various literals are minified correctly.

```lua
local x

x = 0
x = 0., .0, 0.0
x = 0. .. .0
x = - - 2
x = 2 and 2
do local x = 0 end

x = "hello"
x = 'hello'
x = [[hello]]
x = x[ [[hello]] ]
```

```txt
local e e=0 e=0.,.0,0.0 e=0. .. .0 e=- -2 e=2 and 2 do local e=0 end
e="hello"e='hello'e=[[hello]]e=e[
[[hello]]]
```
