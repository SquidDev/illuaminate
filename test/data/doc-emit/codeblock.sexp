((body (id foo) (title foo)
  (contents
   (table
    (a
     ((description "A basic variable\n\n")
      (body expr (ty ((name number))) (value 1))
      (example "```lua\nlocal bar = require \"foo\"\n```\n")
      (example "```lua\nlocal foo = require \"foo\"\nprint(foo.a)\n```\n")))))))