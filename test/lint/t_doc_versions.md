See [`lint_doc_versions.ml`](../../src/lint/lint_doc_versions.ml)

```lua
-- config: (lint (only doc:unordered-versions))

--- A module
--
-- @since 1.0
-- @changed 1.1
-- @since 1.2
```

```txt
in.lua: @since tag for version "1.2" must be the first changelog entry. [doc:unordered-versions]
   │
 7 │ -- @since 1.2
   │    ^^^^^^
```
