((description "This is a simple module which contains a function\n\n")
 (body (id fancy.module) (title fancy.module)
  (contents
   (table (a ((description "A module method\n\n") (body (args))))
    (b ((description "A module method\n\n") (body (args))))))
  (type
   (body (name T)
    (member (name b)
     (value
      ((description "A module method\n\n") (body ((args)))
       (see (ref ((in-module library!fancy.module) (name ty:T:a)))
        (label T.a) (span alias.lua[12:20-12:22])))))
    (member (name a)
     (value ((description "A type method\n\n") (body ((args))))))))))