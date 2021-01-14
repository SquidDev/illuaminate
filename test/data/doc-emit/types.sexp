((body (id types) (title types)
  (contents
   (table
    (no_clue ((description "A basic exported *value*.\n\n") (body table)))
    (ty_1 ((body (args (arg (name a)) (arg (name b))))))
    (ty_2
     ((description "Function type syntax\n\n")
      (body
       (args
        (arg (name a)
         (type
          (union "function(...)"
           ((name string)
            (url https://www.lua.org/manual/5.1/manual.html#5.4)))))
        (arg (name b) (type "function(...)"))
        (arg (name c) (type "function(...)"))
        (arg (name d) (type "function(...)"))
        (arg (name e) (type "function(...)"))
        (arg (name f) (type "function(...)"))
        (arg (name g) (type "function(...)"))))))
    (ty_3
     ((description "Table syntax\n\n")
      (body
       (args (arg (name a) (type {...})) (arg (name b) (type {...}))
        (arg (name c) (type {...}))))))
    (ref
     ((description "Type references\n\n")
      (body
       (args (arg (name a) (type no_clue))
        (arg (name b) (type ((in-module library!types) (name ty:Foo))))))))
    (opt
     ((description "Optional arguments\n\n")
      (body
       (args
        (arg (name a) (opt)
         (type
          ((name string)
           (url https://www.lua.org/manual/5.1/manual.html#5.4))))
        (arg (name b) (opt 2) (type ((name number))))))))))
  (type (body (name Foo)))))