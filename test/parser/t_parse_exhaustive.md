An exhaustive list of all error states in the parser, and the error messages we
generate for each one. This is _not_ a complete collection of all possible
errors, but is a useful guide for where we might be providing terrible messages.

```lua
do true
```

```txt
=input: Unexpected `true`. Expected `end` or another statement. [parse:syntax-error]
   │
 1 │ do true
   │ ^^ Block started here.
   │
 1 │ do true
   │    ^^^^ Expected end of block here.
```

```lua
:: goto while
```

```txt
=input: Unexpected `while`. [parse:syntax-error]
   │
 1 │ :: goto while
   │ ^^ Label was started here.
   │
 1 │ :: goto while
   │         ^^^^^ Tip: Try adding `::` here.
```

```lua
:: while
```

```txt
=input: Unexpected `while`: Expected a label after `::`. [parse:syntax-error]
   │
 1 │ :: while
   │    ^^^^^
(from messages.txt)
```

```lua
for goto , goto while
```

```txt
=input: Unexpected `while`: Expected `in` after for loop variables [parse:syntax-error]
   │
 1 │ for goto , goto while
   │                 ^^^^^
(from messages.txt)
```

```lua
for goto , while
```

```txt
=input: Unexpected `while`. Expected a variable name. [parse:syntax-error]
   │
 1 │ for goto , while
   │            ^^^^^
```

```lua
for goto = goto , goto , goto while
```

```txt
=input: Unexpected `while`: Expected `do` after for loop initialisation
We should have something of the form
 - for i = 1, 10, 1 do ... end [parse:syntax-error]
   │
 1 │ for goto = goto , goto , goto while
   │                               ^^^^^
(from messages.txt)
```

```lua
for goto = goto , goto , while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ for goto = goto , goto , while
   │                          ^^^^^
```

```lua
for goto = goto , goto do true
```

```txt
=input: Unexpected `true`. Expected a statement. [parse:syntax-error]
   │
 1 │ for goto = goto , goto do true
   │                           ^^^^
```

```lua
for goto = goto , goto while
```

```txt
=input: Unexpected `while`: Expected `do` to finish for loop initialisation.
We expect something of the form
 - for i = 1, 10 do end or
 - for i = 1, 10, 1 do end [parse:syntax-error]
   │
 1 │ for goto = goto , goto while
   │                        ^^^^^
(from messages.txt)
```

```lua
for goto = goto , while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ for goto = goto , while
   │                   ^^^^^
```

```lua
for goto = goto while
```

```txt
=input: Unexpected `while`: Expected comma after for loop start [parse:syntax-error]
   │
 1 │ for goto = goto while
   │                 ^^^^^
(from messages.txt)
```

```lua
for goto = while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ for goto = while
   │            ^^^^^
```

```lua
for goto in goto do true
```

```txt
=input: Unexpected `true`. Expected a statement. [parse:syntax-error]
   │
 1 │ for goto in goto do true
   │                     ^^^^
```

```lua
for goto in goto while
```

```txt
=input: Unexpected `while`: Expected `do` after for loop initialiser [parse:syntax-error]
   │
 1 │ for goto in goto while
   │                  ^^^^^
(from messages.txt)
```

```lua
for goto in while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ for goto in while
   │             ^^^^^
```

```lua
for goto while
```

```txt
=input: Unexpected `while`: Expected `=` or `in` after `for` loop variables [parse:syntax-error]
   │
 1 │ for goto while
   │          ^^^^^
(from messages.txt)
```

```lua
for while
```

```txt
=input: Unexpected `while`. Expected a variable name. [parse:syntax-error]
   │
 1 │ for while
   │     ^^^^^
```

```lua
function goto : while
```

```txt
=input: Unexpected `while`: Expected method name after `:`. [parse:syntax-error]
   │
 1 │ function goto : while
   │                 ^^^^^
(from messages.txt)
```

```lua
function goto . while
```

```txt
=input: Unexpected `while`: Expected function name after `.`. [parse:syntax-error]
   │
 1 │ function goto . while
   │                 ^^^^^
(from messages.txt)
```

```lua
function goto ( ) true
```

```txt
=input: Unexpected `true`. Expected `end` or another statement. [parse:syntax-error]
   │
 1 │ function goto ( ) true
   │ ^^^^^^^^ Block started here.
   │
 1 │ function goto ( ) true
   │                   ^^^^ Expected end of block here.
```

```lua
function goto while
```

```txt
=input: Unexpected `while`. Expected `(` to start function arguments. [parse:syntax-error]
   │
 1 │ function goto while
   │               ^^^^^
```

```lua
function while
```

```txt
=input: Unexpected `while`. Expected a variable name. [parse:syntax-error]
   │
 1 │ function while
   │          ^^^^^
```

```lua
goto , while
```

```txt
=input: Unexpected `while`. Expected a variable name. [parse:syntax-error]
   │
 1 │ goto , while
   │        ^^^^^
```

```lua
goto = while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto = while
   │        ^^^^^
```

```lua
goto 'abc' true
```

```txt
=input: Unexpected `true`. Expected a statement. [parse:syntax-error]
   │
 1 │ goto 'abc' true
   │            ^^^^
```

```lua
goto while
```

```txt
=input: Unexpected `while` after name. [parse:syntax-error]
   │
 1 │ goto while
   │      ^^^^^
Did you mean to assign this or call it as a function?
```

```lua
xyz while
```

```txt
=input: Unexpected `while` after name. [parse:syntax-error]
   │
 1 │ xyz while
   │     ^^^^^
Did you mean to assign this or call it as a function?
```

```lua
if goto then else true
```

```txt
=input: Unexpected `true`. Expected `end` or another statement. [parse:syntax-error]
   │
 1 │ if goto then else true
   │              ^^^^ Block started here.
   │
 1 │ if goto then else true
   │                   ^^^^ Expected end of block here.
```

```lua
if goto then elseif goto then true
```

```txt
=input: Unexpected `true`. Expected `end` or another statement. [parse:syntax-error]
   │
 1 │ if goto then elseif goto then true
   │              ^^^^^^ Block started here.
   │
 1 │ if goto then elseif goto then true
   │                               ^^^^ Expected end of block here.
```

```lua
if goto then elseif goto while
```

```txt
=input: Expected `then` after if condition. [parse:syntax-error]
   │
 1 │ if goto then elseif goto while
   │              ^^^^^^ If statement started here.
   │
 1 │ if goto then elseif goto while
   │                          ^^^^^ Expected `then` before here.
```

```lua
if goto then elseif while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ if goto then elseif while
   │                     ^^^^^
```

```lua
if goto then true
```

```txt
=input: Unexpected `true`. Expected `end` or another statement. [parse:syntax-error]
   │
 1 │ if goto then true
   │ ^^ Block started here.
   │
 1 │ if goto then true
   │              ^^^^ Expected end of block here.
```

```lua
if goto while
```

```txt
=input: Expected `then` after if condition. [parse:syntax-error]
   │
 1 │ if goto while
   │ ^^ If statement started here.
   │
 1 │ if goto while
   │         ^^^^^ Expected `then` before here.
```

```lua
if while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ if while
   │    ^^^^^
```

```lua
local function goto ( ) true
```

```txt
=input: Unexpected `true`. Expected `end` or another statement. [parse:syntax-error]
   │
 1 │ local function goto ( ) true
   │ ^^^^^^^^^^^^^^ Block started here.
   │
 1 │ local function goto ( ) true
   │                         ^^^^ Expected end of block here.
```

```lua
local function goto while
```

```txt
=input: Unexpected `while`. Expected `(` to start function arguments. [parse:syntax-error]
   │
 1 │ local function goto while
   │                     ^^^^^
```

```lua
local function while
```

```txt
=input: Unexpected `while`. Expected a variable name. [parse:syntax-error]
   │
 1 │ local function while
   │                ^^^^^
```

```lua
local goto = while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ local goto = while
   │              ^^^^^
```

```lua
local goto in
```

```txt
=input: Unexpected `in`. Expected a statement. [parse:syntax-error]
   │
 1 │ local goto in
   │            ^^
```

```lua
local goto true
```

```txt
=input: Unexpected `true`. Expected a statement. [parse:syntax-error]
   │
 1 │ local goto true
   │            ^^^^
```

```lua
local while
```

```txt
=input: Unexpected `while`. Expected a variable name. [parse:syntax-error]
   │
 1 │ local while
   │       ^^^^^
```

```lua
( goto ) while
```

```txt
=input: Unexpected `while`: We've just finished parsing a simple expression. [parse:syntax-error]
   │
 1 │ ( goto ) while
   │          ^^^^^
(from messages.txt)
```

```lua
repeat true
```

```txt
=input: Unexpected `true`. Expected a statement. [parse:syntax-error]
   │
 1 │ repeat true
   │        ^^^^
```

```lua
repeat until goto then
```

```txt
=input: Unexpected `then`. Expected a statement. [parse:syntax-error]
   │
 1 │ repeat until goto then
   │                   ^^^^
```

```lua
repeat until while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ repeat until while
   │              ^^^^^
```

```lua
return then
```

```txt
=input: Unexpected `then`. Expected a statement. [parse:syntax-error]
   │
 1 │ return then
   │        ^^^^
```

```lua
true
```

```txt
=input: Unexpected `true`. Expected a statement. [parse:syntax-error]
   │
 1 │ true
   │ ^^^^
```

```lua
while goto do true
```

```txt
=input: Unexpected `true`. Expected `end` or another statement. [parse:syntax-error]
   │
 1 │ while goto do true
   │ ^^^^^ Block started here.
   │
 1 │ while goto do true
   │               ^^^^ Expected end of block here.
```

```lua
while goto while
```

```txt
=input: Unexpected `while`: Expected `do` after `while` loop condition. [parse:syntax-error]
   │
 1 │ while goto while
   │            ^^^^^
(from messages.txt)
```

```lua
while while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ while while
   │       ^^^^^
```

```lua{repl_exprs}
... true
```

```txt
=input: Unexpected `true`: Unexpected symbol after expression. [parse:syntax-error]
   │
 1 │ ... true
   │     ^^^^
(from messages.txt)
```

```lua{repl_exprs}
function ( ) true
```

```txt
=input: Unexpected `true`. Expected `end` or another statement. [parse:syntax-error]
   │
 1 │ function ( ) true
   │ ^^^^^^^^ Block started here.
   │
 1 │ function ( ) true
   │              ^^^^ Expected end of block here.
```

```lua{repl_exprs}
function ( goto , while
```

```txt
=input: Unexpected `while`. Expected a variable name. [parse:syntax-error]
   │
 1 │ function ( goto , while
   │                   ^^^^^
```

```lua{repl_exprs}
function ( goto while
```

```txt
=input: Unexpected `while`. Are you missing a closing bracket? [parse:syntax-error]
   │
 1 │ function ( goto while
   │          ^ Brackets were opened here.
   │
 1 │ function ( goto while
   │                 ^^^^^ Unexpected `while` here.
```

```lua{repl_exprs}
function ( while
```

```txt
=input: Unexpected `while`. Are you missing a closing bracket? [parse:syntax-error]
   │
 1 │ function ( while
   │          ^ Brackets were opened here.
   │
 1 │ function ( while
   │            ^^^^^ Unexpected `while` here.
```

```lua{repl_exprs}
function while
```

```txt
=input: Unexpected `while`. Expected `(` to start function arguments. [parse:syntax-error]
   │
 1 │ function while
   │          ^^^^^
```

```lua{repl_exprs}
goto + while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto + while
   │        ^^^^^
```

```lua{repl_exprs}
goto and while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto and while
   │          ^^^^^
```

```lua{repl_exprs}
goto : goto while
```

```txt
=input: Unexpected `while`: Expected argument in self-call. [parse:syntax-error]
   │
 1 │ goto : goto while
   │             ^^^^^
(from messages.txt)
```

```lua{repl_exprs}
goto : while
```

```txt
=input: Unexpected `while`: Expected method name in self-call. [parse:syntax-error]
   │
 1 │ goto : while
   │        ^^^^^
(from messages.txt)
```

```lua{repl_exprs}
goto , while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto , while
   │        ^^^^^
```

```lua{repl_exprs}
goto .. while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto .. while
   │         ^^^^^
```

```lua{repl_exprs}
goto / while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto / while
   │        ^^^^^
```

```lua{repl_exprs}
goto . while
```

```txt
=input: Unexpected `while`: Expected table key after `.`. [parse:syntax-error]
   │
 1 │ goto . while
   │        ^^^^^
(from messages.txt)
```

```lua{repl_exprs}
goto == while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto == while
   │         ^^^^^
```

```lua{repl_exprs}
goto >= while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto >= while
   │         ^^^^^
```

```lua{repl_exprs}
goto > while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto > while
   │        ^^^^^
```

```lua{repl_exprs}
goto <= while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto <= while
   │         ^^^^^
```

```lua{repl_exprs}
goto < while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto < while
   │        ^^^^^
```

```lua{repl_exprs}
goto % while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto % while
   │        ^^^^^
```

```lua{repl_exprs}
goto * while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto * while
   │        ^^^^^
```

```lua{repl_exprs}
goto ~= while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto ~= while
   │         ^^^^^
```

```lua{repl_exprs}
goto ( goto while
```

```txt
=input: Unexpected `while`. Are you missing a closing bracket? [parse:syntax-error]
   │
 1 │ goto ( goto while
   │      ^ Brackets were opened here.
   │
 1 │ goto ( goto while
   │             ^^^^^ Unexpected `while` here.
```

```lua{repl_exprs}
goto ( while
```

```txt
=input: Unexpected `while`. Are you missing a closing bracket? [parse:syntax-error]
   │
 1 │ goto ( while
   │      ^ Brackets were opened here.
   │
 1 │ goto ( while
   │        ^^^^^ Unexpected `while` here.
```

```lua{repl_exprs}
goto or while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto or while
   │         ^^^^^
```

```lua{repl_exprs}
goto [ goto while
```

```txt
=input: Unexpected `while`. Are you missing a closing bracket? [parse:syntax-error]
   │
 1 │ goto [ goto while
   │      ^ Brackets were opened here.
   │
 1 │ goto [ goto while
   │             ^^^^^ Unexpected `while` here.
```

```lua{repl_exprs}
goto [ while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto [ while
   │        ^^^^^
```

```lua{repl_exprs}
goto ^ while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto ^ while
   │        ^^^^^
```

```lua{repl_exprs}
goto - while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ goto - while
   │        ^^^^^
```

```lua{repl_exprs}
goto then
```

```txt
=input: Unexpected `then`: Unexpected symbol after expression. [parse:syntax-error]
   │
 1 │ goto then
   │      ^^^^
(from messages.txt)
```

```lua{repl_exprs}
goto true
```

```txt
=input: Unexpected `true`: This is unexpected after a simple expression. [parse:syntax-error]
   │
 1 │ goto true
   │      ^^^^
(from messages.txt)
```

```lua{repl_exprs}
goto while
```

```txt
=input: Unexpected `while`: Expected eof after an expression. [parse:syntax-error]
   │
 1 │ goto while
   │      ^^^^^
(from messages.txt)
```

```lua{repl_exprs}
# while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ # while
   │   ^^^^^
```

```lua{repl_exprs}
not while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ not while
   │     ^^^^^
```

```lua{repl_exprs}
{ goto = goto while
```

```txt
=input: Unexpected `while`. Are you missing a closing bracket? [parse:syntax-error]
   │
 1 │ { goto = goto while
   │ ^ Brackets were opened here.
   │
 1 │ { goto = goto while
   │               ^^^^^ Unexpected `while` here.
```

```lua{repl_exprs}
{ goto = while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ { goto = while
   │          ^^^^^
```

```lua{repl_exprs}
{ goto ; while
```

```txt
=input: Unexpected `while`. Are you missing a closing bracket? [parse:syntax-error]
   │
 1 │ { goto ; while
   │ ^ Brackets were opened here.
   │
 1 │ { goto ; while
   │          ^^^^^ Unexpected `while` here.
```

```lua{repl_exprs}
{ goto while
```

```txt
=input: Unexpected `while`. Are you missing a closing bracket? [parse:syntax-error]
   │
 1 │ { goto while
   │ ^ Brackets were opened here.
   │
 1 │ { goto while
   │        ^^^^^ Unexpected `while` here.
```

```lua{repl_exprs}
{ [ goto ] = goto while
```

```txt
=input: Unexpected `while`. Are you missing a closing bracket? [parse:syntax-error]
   │
 1 │ { [ goto ] = goto while
   │ ^ Brackets were opened here.
   │
 1 │ { [ goto ] = goto while
   │                   ^^^^^ Unexpected `while` here.
```

```lua{repl_exprs}
{ [ goto ] = while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ { [ goto ] = while
   │              ^^^^^
```

```lua{repl_exprs}
{ [ goto ] while
```

```txt
=input: Unexpected `while`: Expected `=` after table key. [parse:syntax-error]
   │
 1 │ { [ goto ] while
   │            ^^^^^
(from messages.txt)
```

```lua{repl_exprs}
{ [ goto while
```

```txt
=input: Unexpected `while`: Expected `]` to close `[` in table key. [parse:syntax-error]
   │
 1 │ { [ goto while
   │          ^^^^^
(from messages.txt)
```

```lua{repl_exprs}
{ [ while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ { [ while
   │     ^^^^^
```

```lua{repl_exprs}
{ while
```

```txt
=input: Unexpected `while`. Are you missing a closing bracket? [parse:syntax-error]
   │
 1 │ { while
   │ ^ Brackets were opened here.
   │
 1 │ { while
   │   ^^^^^ Unexpected `while` here.
```

```lua{repl_exprs}
( goto while
```

```txt
=input: Unexpected `while`. Are you missing a closing bracket? [parse:syntax-error]
   │
 1 │ ( goto while
   │ ^ Brackets were opened here.
   │
 1 │ ( goto while
   │        ^^^^^ Unexpected `while` here.
```

```lua{repl_exprs}
( while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ ( while
   │   ^^^^^
```

```lua{repl_exprs}
- while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ - while
   │   ^^^^^
```

```lua{repl_exprs}
while
```

```txt
=input: Unexpected `while`. Expected an expression. [parse:syntax-error]
   │
 1 │ while
   │ ^^^^^
```
