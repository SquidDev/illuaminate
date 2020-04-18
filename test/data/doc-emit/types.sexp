((body (name types)
  (contents
   (table
    (no_clue ((description "A basic exported *value*.\n\n") (body table)))
    (ty_1 ((body (args ((name a)) ((name b))))))
    (ty_2
     ((description "Function type syntax\n\n")
      (body
       (args
        ((name a)
         (type union (union "function(...)")
          (union
           ((name string)
            (url https://www.lua.org/manual/5.1/manual.html#5.4))))
         (description )) ((name b) (type "function(...)") (description ))
        ((name c) (type "function(...)") (description ))
        ((name d) (type "function(...)") (description ))
        ((name e) (type "function(...)") (description ))
        ((name f) (type "function(...)") (description ))
        ((name g) (type "function(...)") (description ))))))
    (ty_3
     ((description "Table syntax\n\n")
      (body
       (args ((name a) (type {...}) (description ))
        ((name b) (type {...}) (description ))
        ((name c) (type {...}) (description ))))))
    (ref
     ((description "Type references\n\n")
      (body
       (args ((name a) (type no_clue) (description ))
        ((name b) (type ((in-module types) (name ty:Foo))) (description ))))))
    (opt
     ((description "Optional arguments\n\n")
      (body
       (args
        ((name a) (opt)
         (type
          ((name string)
           (url https://www.lua.org/manual/5.1/manual.html#5.4)))
         (description )))))))) (type (body (name Foo)))))