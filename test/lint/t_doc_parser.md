See [`lint_doc_parser.ml`](../../src/lint/lint_doc_parser.ml)

# Unknown module kinds
```lua
-- config: (doc (module-kinds (kind1 "Kind 1")))

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
--- A module
--
-- @module foo
-- @module bar
```

```txt
in.lua: Duplicate @module definitions (named 'foo' and 'bar') [doc:duplicate-definitions]
   │
 4 │ -- @module bar
   │    ^^^^^^^
```

# Unknown tags and flags
```lua
--- @module[abc, abc=xyz]

--- @unknown

  --[[- Test this all works in an indented block comment
  @unknown
  ]]
```

```txt
in.lua: @module has unknown flag 'abc' [doc:unknown-flag]
   │
 1 │ --- @module[abc, abc=xyz]
   │             ^^^

in.lua: @module has unknown flag 'abc' [doc:unknown-flag]
   │
 1 │ --- @module[abc, abc=xyz]
   │                  ^^^

in.lua: Unknown tag @unknown [doc:unknown-tag]
   │
 3 │ --- @unknown
   │     ^^^^^^^^

in.lua: Unknown tag @unknown [doc:unknown-tag]
   │
 6 │   @unknown
   │   ^^^^^^^^
```
