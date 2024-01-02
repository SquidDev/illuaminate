We provide a parser for Lua source code. Here we test that the parser reports
sensible syntax errors in specific cases.

# Expressions

## Invalid equals
We correct the user if they type `=` instead of `==`.

```lua
if a = b then end
```

```txt
=input: Unexpected `=` in expression. [parse:syntax-error]
   │
 1 │ if a = b then end
   │      ^
Tip: Replace this with `==` to check if two values are equal.
```

We apply a slightly different error when this occurs in tables:

```lua
return { "abc" = "def" }
```

```txt
=input: Unexpected `=` in expression. [parse:syntax-error]
   │
 1 │ return { "abc" = "def" }
   │                ^
Tip: Wrap the preceding expression in `[` and `]` to use it as a table key.
```

Note this doesn't occur if this there's already a table key here:

```lua
return { x = "abc" = }
```

```txt
=input: Unexpected `=` in expression. [parse:syntax-error]
   │
 1 │ return { x = "abc" = }
   │                    ^
Tip: Replace this with `==` to check if two values are equal.
```

## Unclosed parenthesis
We warn on unclosed parenthesis in expressions:

```lua
return (2
```

```txt
=input: Unexpected end of file. Are you missing a closing bracket? [parse:syntax-error]
   │
 1 │ return (2
   │        ^ Brackets were opened here.
   │
 1 │ return (2
   │          ^ Unexpected end of file here.
```

Function calls:

```lua
return f(2
```

```txt
=input: Unexpected end of file. Are you missing a closing bracket? [parse:syntax-error]
   │
 1 │ return f(2
   │         ^ Brackets were opened here.
   │
 1 │ return f(2
   │           ^ Unexpected end of file here.
```

and function definitions:

```lua
local function f(a
```

```txt
=input: Unexpected end of file. Are you missing a closing bracket? [parse:syntax-error]
   │
 1 │ local function f(a
   │                 ^ Brackets were opened here.
   │
 1 │ local function f(a
   │                   ^ Unexpected end of file here.
```

## Missing commas in tables

```lua
return { 1 2 }
```

```txt
=input: Unexpected number in table. [parse:syntax-error]
   │
 1 │ return { 1 2 }
   │            ^
   │
 1 │ return { 1 2 }
   │           ^ Are you missing a comma here?
```
```lua
return { 1, 2 3 }
```

```txt
=input: Unexpected number in table. [parse:syntax-error]
   │
 1 │ return { 1, 2 3 }
   │               ^
   │
 1 │ return { 1, 2 3 }
   │              ^ Are you missing a comma here?
```
```lua
print({ 1, )
```

```txt
=input: Unexpected `)`. Are you missing a closing bracket? [parse:syntax-error]
   │
 1 │ print({ 1, )
   │       ^ Brackets were opened here.
   │
 1 │ print({ 1, )
   │            ^ Unexpected `)` here.
```

# Statements

## Local functions with table identifiers
We provide a custom error for using `.` inside a `local function` name.

```lua
local function x.f() end
```

```txt
=input: Cannot use `local function` with a table key. [parse:syntax-error]
   │
 1 │ local function x.f() end
   │                 ^ `.` appears here.
   │
 1 │ local function x.f() end
   │ ^^^^^ Tip: Try removing this `local` keyword.
```

## Standalone identifiers
A common error is a user forgetting to use `()` to call a function. We provide
a custom error for this case:

```lua
term.clear
local _ = 1
```

```txt
=input: Unexpected symbol after name. [parse:syntax-error]
   │
 1 │ term.clear
   │           ^ Expected something before the end of the line.
Tip: Use `()` to call with no arguments.
```

If the next symbol is on the same line we provide a slightly different error:

```lua
x 1
```

```txt
=input: Unexpected number after name. [parse:syntax-error]
   │
 1 │ x 1
   │   ^
Did you mean to assign this or call it as a function?
```

An EOF token is treated as a new line.

```lua
term.clear
```

```txt
=input: Unexpected symbol after name. [parse:syntax-error]
   │
 1 │ term.clear
   │           ^ Expected something before the end of the line.
Tip: Use `()` to call with no arguments.
```

## If statements
For if statements, we say when we expected the `then` keyword.

```lua
if 0
```

```txt
=input: Expected `then` after if condition. [parse:syntax-error]
   │
 1 │ if 0
   │ ^^ If statement started here.
   │
 1 │ if 0
   │     ^ Expected `then` before here.
```

```lua
if 0 then
elseif 0
```

```txt
=input: Expected `then` after if condition. [parse:syntax-error]
   │
 2 │ elseif 0
   │ ^^^^^^ If statement started here.
   │
 2 │ elseif 0
   │         ^ Expected `then` before here.
```

## Expecting `end`
We provide errors for missing `end`s.

```lua
if true then
  print("Hello")
```

```txt
=input: Unexpected end of file. Expected `end` or another statement. [parse:syntax-error]
   │
 1 │ if true then
   │ ^^ Block started here.
   │
 2 │   print("Hello")
   │                 ^ Expected end of block here.
```

```lua
if true then
else
  print("Hello")
```

```txt
=input: Unexpected end of file. Expected `end` or another statement. [parse:syntax-error]
   │
 2 │ else
   │ ^^^^ Block started here.
   │
 3 │   print("Hello")
   │                 ^ Expected end of block here.
```

```lua
if true then
elseif true then
  print("Hello")
```

```txt
=input: Unexpected end of file. Expected `end` or another statement. [parse:syntax-error]
   │
 2 │ elseif true then
   │ ^^^^^^ Block started here.
   │
 3 │   print("Hello")
   │                 ^ Expected end of block here.
```

```lua
while true do
  print("Hello")
```

```txt
=input: Unexpected end of file. Expected `end` or another statement. [parse:syntax-error]
   │
 1 │ while true do
   │ ^^^^^ Block started here.
   │
 2 │   print("Hello")
   │                 ^ Expected end of block here.
```

```lua
local function f()
```

```txt
=input: Unexpected end of file. Expected `end` or another statement. [parse:syntax-error]
   │
 1 │ local function f()
   │ ^^^^^^^^^^^^^^ Block started here.
   │
 1 │ local function f()
   │                   ^ Expected end of block here.
```

```lua
function f()
```

```txt
=input: Unexpected end of file. Expected `end` or another statement. [parse:syntax-error]
   │
 1 │ function f()
   │ ^^^^^^^^ Block started here.
   │
 1 │ function f()
   │             ^ Expected end of block here.
```

```lua
return function()
```

```txt
=input: Unexpected end of file. Expected `end` or another statement. [parse:syntax-error]
   │
 1 │ return function()
   │        ^^^^^^^^ Block started here.
   │
 1 │ return function()
   │                  ^ Expected end of block here.
```

While we typically see these errors at the end of the file, there are some cases
where it may occur before then:

```lua
return (function()
  if true then
)()
```

```txt
=input: Unexpected `)`. Expected `end` or another statement. [parse:syntax-error]
   │
 2 │   if true then
   │   ^^ Block started here.
   │
 3 │ )()
   │ ^ Expected end of block here.
```

Note we do not currently attempt to identify mismatched `end`s. This might be
something to do in the future.

```lua
if true then
  while true do
end
```

```txt
=input: Unexpected end of file. Expected `end` or another statement. [parse:syntax-error]
   │
 1 │ if true then
   │ ^^ Block started here.
   │
 3 │ end
   │    ^ Expected end of block here.
```

## Unexpected `end`
We also print when there's more `end`s than expected.

```lua
if true then
end
end
```

```txt
=input: Unexpected `end`. [parse:syntax-error]
   │
 3 │ end
   │ ^^^
Your program contains more `end`s than needed. Check each block (`if`, `for`, `function`, ...) only has one `end`
```

```lua
repeat
  if true then
  end
  end
until true
```

```txt
=input: Unexpected `end`. [parse:syntax-error]
   │
 4 │   end
   │   ^^^
Your program contains more `end`s than needed. Check each block (`if`, `for`, `function`, ...) only has one `end`
```

## `goto` and labels
We `goto` the same as normal identifiers.

```lua
goto 2
```

```txt
=input: Unexpected number after name. [parse:syntax-error]
   │
 1 │ goto 2
   │      ^
Did you mean to assign this or call it as a function?
```

Labels have a basic closing check:
```lua
::foo
```

```txt
=input: Unexpected end of file. [parse:syntax-error]
   │
 1 │ ::foo
   │ ^^ Label was started here.
   │
 1 │ ::foo
   │      ^ Tip: Try adding `::` here.
```

But we do nothing fancy for just a `::`

```lua
::
```

```txt
=input: Unexpected end of file: Expected a label after `::`. [parse:syntax-error]
   │
 1 │ ::
   │   ^
(from messages.txt)
```

# Function calls

## Additional commas
We suggest the user removes additional trailing commas on function calls:

```lua
f(2, )
```

```txt
=input: Unexpected `)` in function call. [parse:syntax-error]
   │
 1 │ f(2, )
   │      ^
   │
 1 │ f(2, )
   │    ^ Tip: Try removing this `,`.
```

```lua
f(2, 3, )
```

```txt
=input: Unexpected `)` in function call. [parse:syntax-error]
   │
 1 │ f(2, 3, )
   │         ^
   │
 1 │ f(2, 3, )
   │       ^ Tip: Try removing this `,`.
```

```lua
x:f(2, 3, )
```

```txt
=input: Unexpected `)` in function call. [parse:syntax-error]
   │
 1 │ x:f(2, 3, )
   │           ^
   │
 1 │ x:f(2, 3, )
   │         ^ Tip: Try removing this `,`.
```
