-- Calls
f(a)
f { a }
f "a"
x:f()

-- Expressions
f(a + b, -a, { [a] = a, a, a = a }, (((a))))
f(f("Hello"))
f(1, 1.1, 1..1, true, false, "hello", nil, ...)

-- Function names
function f() end
function f.x() end
function f:x() end

-- Index names
x = x
x.y = x.y.z
x[y] = x[y]
x[y + 2] = x[y + 2]

-- Control flow
for i = x, y do local x end
for i = x, y, z do local x end
for k, v in k, v do local x end
for k in k do ; break end

while x do local x end
repeat local x until x
if x then local x end
if x then local x else local x end
if x then local x elseif x then local x end
if x then local x elseif x then local x else local x end
if x then local x elseif x then local x elseif x then local x else local x end

do local x end
do return x end

-- Assignments
local x = y
local x, y, z = x, y, z

-- Functions
local function f(a, b, ...) local x end
function f(a, b, ...) local x end
local f = function(a, b, ...) local x end
