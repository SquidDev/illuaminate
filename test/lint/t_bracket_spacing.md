See [`lint_bracket_spacing.ml`](../../src/lint/lint_bracket_spacing.ml)

# In function calls
```lua
-- config: (lint (only format:bracket-space))

print(1 ) print( 1) local _ = print(1 )
```

```txt
in.lua: Inconsistent spacing [format:bracket-space]
   │
 3 │ print(1 ) print( 1) local _ = print(1 )
   │      ^^^^
No space after the opening bracket, but spaces before the closing one.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 3 │ print(1 ) print( 1) local _ = print(1 )
   │                ^^^^
Spaces after the opening bracket, but no spaces before the closing one.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 3 │ print(1 ) print( 1) local _ = print(1 )
   │                                    ^^^^
No space after the opening bracket, but spaces before the closing one.
```

```diff
@ -2,3 +2,3 @
  -- config: (lint (only format:bracket-space))

- print(1 ) print( 1) local _ = print(1 )
+ print( 1 ) print( 1 ) local _ = print( 1 )
```

# In brackets
```lua
-- config: (lint (only format:bracket-space))

local _ = ( 1), (1 )
```

```txt
in.lua: Inconsistent spacing [format:bracket-space]
   │
 3 │ local _ = ( 1), (1 )
   │           ^^^^
Spaces after the opening bracket, but no spaces before the closing one.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 3 │ local _ = ( 1), (1 )
   │                 ^^^^
No space after the opening bracket, but spaces before the closing one.
```

```diff
@ -2,3 +2,3 @
  -- config: (lint (only format:bracket-space))

- local _ = ( 1), (1 )
+ local _ = ( 1 ), ( 1 )
```

# In tables
```lua
-- config: (lint (only format:bracket-space))

local _ = { 1}, {1 }
local _ = { [1 ] = true }, { [ 1] = true }
local _ = x[1 ], x[ 1]
```

```txt
in.lua: Inconsistent spacing [format:bracket-space]
   │
 3 │ local _ = { 1}, {1 }
   │           ^^^^
Spaces after the opening bracket, but no spaces before the closing one.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 3 │ local _ = { 1}, {1 }
   │                 ^^^^
No space after the opening bracket, but spaces before the closing one.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 4 │ local _ = { [1 ] = true }, { [ 1] = true }
   │             ^^^^
No space after the opening bracket, but spaces before the closing one.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 4 │ local _ = { [1 ] = true }, { [ 1] = true }
   │                              ^^^^
Spaces after the opening bracket, but no spaces before the closing one.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 5 │ local _ = x[1 ], x[ 1]
   │            ^^^^
No space after the opening bracket, but spaces before the closing one.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 5 │ local _ = x[1 ], x[ 1]
   │                   ^^^^
Spaces after the opening bracket, but no spaces before the closing one.
```

```diff
@ -2,5 +2,5 @
  -- config: (lint (only format:bracket-space))

- local _ = { 1}, {1 }
- local _ = { [1 ] = true }, { [ 1] = true }
- local _ = x[1 ], x[ 1]
+ local _ = { 1 }, { 1 }
+ local _ = { [ 1 ] = true }, { [ 1 ] = true }
+ local _ = x[ 1 ], x[ 1 ]
```

# In function arguments
```lua
-- config: (lint (only format:bracket-space))
local function f( x) end
local function f(x ) end

function f( x) end
function f(x ) end

local f = function( x) end
local f = function(x ) end
```

```txt
in.lua: Inconsistent spacing [format:bracket-space]
   │
 2 │ local function f( x) end
   │                 ^^^^
Spaces after the opening bracket, but no spaces before the closing one.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 3 │ local function f(x ) end
   │                 ^^^^
No space after the opening bracket, but spaces before the closing one.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 5 │ function f( x) end
   │           ^^^^
Spaces after the opening bracket, but no spaces before the closing one.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 6 │ function f(x ) end
   │           ^^^^
No space after the opening bracket, but spaces before the closing one.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 8 │ local f = function( x) end
   │                   ^^^^
Spaces after the opening bracket, but no spaces before the closing one.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 9 │ local f = function(x ) end
   │                   ^^^^
No space after the opening bracket, but spaces before the closing one.
```

```diff
@ -1,9 +1,9 @
  -- config: (lint (only format:bracket-space))
- local function f( x) end
- local function f(x ) end
-
- function f( x) end
- function f(x ) end
+ local function f( x ) end
+ local function f( x ) end

- local f = function( x) end
- local f = function(x ) end
+ function f( x ) end
+ function f( x ) end
+
+ local f = function( x ) end
+ local f = function( x ) end
```

# Force no spaces
You can configure the linter to never allow spaces within an item.

```lua
-- config: (lint (only format:bracket-space) (bracket-spaces (table no-space)))

local _ = { 1}, {1 }, {1}, { 1 }
```

```txt
in.lua: Inconsistent spacing [format:bracket-space]
   │
 3 │ local _ = { 1}, {1 }, {1}, { 1 }
   │           ^^^^
Should not have spaces within the brackets.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 3 │ local _ = { 1}, {1 }, {1}, { 1 }
   │                 ^^^^
Should not have spaces within the brackets.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 3 │ local _ = { 1}, {1 }, {1}, { 1 }
   │                            ^^^^^
Should not have spaces within the brackets.
```

```diff
@ -2,3 +2,3 @
  -- config: (lint (only format:bracket-space) (bracket-spaces (table no-space)))

- local _ = { 1}, {1 }, {1}, { 1 }
+ local _ = {1}, {1}, {1}, {1}
```

# Force with spaces
And similarly you can configure it to require spaces.

```lua
-- config: (lint (only format:bracket-space) (bracket-spaces (table space)))

local _ = { 1}, {1 }, {1}, { 1 }
```

```txt
in.lua: Inconsistent spacing [format:bracket-space]
   │
 3 │ local _ = { 1}, {1 }, {1}, { 1 }
   │           ^^^^
Should have spaces within the brackets.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 3 │ local _ = { 1}, {1 }, {1}, { 1 }
   │                 ^^^^
Should have spaces within the brackets.

in.lua: Inconsistent spacing [format:bracket-space]
   │
 3 │ local _ = { 1}, {1 }, {1}, { 1 }
   │                       ^^^
Should have spaces within the brackets.
```

```diff
@ -2,3 +2,3 @
  -- config: (lint (only format:bracket-space) (bracket-spaces (table space)))

- local _ = { 1}, {1 }, {1}, { 1 }
+ local _ = { 1 }, { 1 }, { 1 }, { 1 }
```
