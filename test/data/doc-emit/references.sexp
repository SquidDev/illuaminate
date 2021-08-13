((body (id references) (title references)
  (contents
   (table (my_term ((body table)))
    (unknown_1
     ((body (args))
      (see (ref unknown) (label unknown) (span references.lua[10:12-10:18]))))
    (unknown_2
     ((description
       "<p>See unknown:unknown} and unknown:custom label}.</p>\n")
      (body (args))))
    (term_1
     ((body (args))
      (see (ref ((in-module library!references) (name v:my_term)))
       (label references.my_term) (span references.lua[17:11-17:28]))
      (see (ref ((in-module library!references) (name v:my_term)))
       (label my_term) (span references.lua[16:12-16:18]))))
    (term_2
     ((description
       "<p>See ((in-module library!references) (name v:my_term)):my_term}, ((in-module library!references) (name v:my_term)):references.my_term} and ((in-module library!references) (name v:my_term)):custom label}.</p>\n")
      (body (args))))
    (type_1
     ((body (args))
      (see (ref ((in-module library!references) (name v:my_term)))
       (label references.my_term) (span references.lua[24:11-24:28]))
      (see (ref ((in-module library!references) (name v:my_term)))
       (label my_term) (span references.lua[23:12-23:18]))))
    (type_2
     ((description
       "<p>See ((in-module library!references) (name ty:MyType)):MyType} and ((in-module library!references) (name ty:MyType)):references.MyType}</p>\n")
      (body (args))))
    (type_3
     ((description
       "<p>See ((in-module library!references) (name ty:MyType)):MyType} and ((in-module library!references) (name ty:MyType)):references.MyType}</p>\n")
      (body
       (returns
        (return (type ((in-module library!references) (name ty:MyType))))))))
    (method_1
     ((description
       "<p>See ((in-module library!references) (name ty:MyType:meth)):MyType:meth} and ((in-module library!references) (name ty:MyType:meth)):MyType.meth}</p>\n")
      (body (args))))
    (builtin_1
     ((body (args))
      (see (ref ((name number))) (label number)
       (span references.lua[39:11-39:16]))
      (see
       (ref
        ((name string) (url https://www.lua.org/manual/5.1/manual.html#5.4)))
       (label string) (span references.lua[38:11-38:16]))
      (see
       (ref
        ((name string.match)
         (url https://www.lua.org/manual/5.1/manual.html#pdf-string.match)))
       (label string.match) (span references.lua[37:12-37:23]))))
    (builtin_2
     ((description
       "<p>((name string.match)\n (url https://www.lua.org/manual/5.1/manual.html#pdf-string.match)):string.match}, ((name string) (url https://www.lua.org/manual/5.1/manual.html#5.4)):string} and ((name number)):number}</p>\n")
      (body (args))))
    (module_1
     ((description "<p>((in-module library!references)):references}</p>\n")
      (body (args))))))
  (type
   (body (name MyType)
    (member (name meth) (method) (value ((body ((args) (self))))))))))