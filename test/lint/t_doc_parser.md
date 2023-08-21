See [`lint_doc_parser.ml`](../../src/lint/lint_doc_parser.ml)

# Unknown module kinds
```lua
-- config: (lint (only doc:unknown-module-kind)) (doc (module-kinds (kind1 "Kind 1")))

--- @module[kind=kind1]

--- @module[kind=kind2]
```

```txt
in.lua: Unknown module kind "kind2" [doc:unknown-module-kind]
   │
 5 │ --- @module[kind=kind2]
   │     ^^^^^^^
```

# Duplicate definitions

```lua
-- config: (lint (only doc:duplicate-definitions))

--- A module
--
-- @module foo
-- @module bar
```

```txt
in.lua: Duplicate @module definitions (named 'foo' and 'bar') [doc:duplicate-definitions]
   │
 6 │ -- @module bar
   │    ^^^^^^^
```

# Unknown tags and flags
```lua
-- config: (lint (only doc:unknown-flag doc:unknown-tag))

-- @module[x]

--- @unknown

  --[[- Test this all works in an indented block comment
  @unknown
  ]]
```

```txt
in.lua: Unknown tag @unknown [doc:unknown-tag]
   │
 5 │ --- @unknown
   │     ^^^^^^^^

in.lua: Unknown tag @unknown [doc:unknown-tag]
   │
 8 │   @unknown
   │   ^^^^^^^^
```
