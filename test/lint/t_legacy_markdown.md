See [`lint_legacy_markdown.ml`](../../src/lint/lint_legacy_markdown.ml)

# References
```lua
-- config: (lint (only doc:ldoc-reference))

--- We should replace @{some_reference} and @{some|textural reference} with
-- fancier markdown ones. Note this works @{across|lines}.
```

```txt
in.lua: Use of LDoc reference syntax. [doc:ldoc-reference]
   │
 3 │ --- We should replace @{some_reference} and @{some|textural reference} with
   │                       ^^^^^^^^^^^^^^^^^
Replace with [`some_reference`]

in.lua: Use of LDoc reference syntax. [doc:ldoc-reference]
   │
 3 │ --- We should replace @{some_reference} and @{some|textural reference} with
   │                                             ^^^^^^^^^^^^^^^^^^^^^^^^^^
Replace with [`some`]

in.lua: Use of LDoc reference syntax. [doc:ldoc-reference]
   │
 4 │ -- fancier markdown ones. Note this works @{across|lines}.
   │                                           ^^^^^^^^^^^^^^^
Replace with [`across`]
```

# Admonitions
```lua
-- config: (lint (only doc:docusaurus-admonition))

--- :::warning
--  Some body
--  :::

--[[-
:::warning Oh no
Some body
:::
]]
```

```txt
in.lua: Use of Docusaurus admonition. [doc:docusaurus-admonition]
   │
 3 │ --- :::warning
   │     ^^^^^^^^^^
Replace with a blockquote:
> [!DANGER]
> ...

in.lua: Use of Docusaurus admonition. [doc:docusaurus-admonition]
   │
 8 │ :::warning Oh no
   │ ^^^^^^^^^^^^^^^^
Replace with a blockquote:
> [Oh no][!DANGER]
> ...
```
