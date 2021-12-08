((description "<p>This is a simple module which contains a function</p>\n")
 (body (id fancy.module)
  (contents
   (table (a ((description "<p>A module method</p>\n") (body (args))))
    (b ((description "<p>A module method</p>\n") (body (args))))))
  (type
   (body (name T)
    (member (name b)
     (value
      ((description "<p>A module method</p>\n") (body ((args)))
       (see (ref ((in-module library!fancy.module) (name ty:T:a)))
        (label T.a) (span alias.lua[12:20-12:22])))))
    (member (name a)
     (value ((description "<p>A type method</p>\n") (body ((args))))))))))