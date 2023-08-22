((body (id references)
  (contents
   (table (my_term ((body table)))
    (unknown_1
     ((body (args))
      (see (ref unknown) (label unknown) (span references.lua[10:12-10:18]))))
    (unknown_2
     ((description
       "<p>See [<code>unknown</code>][unknown] and [custom label][unknown].</p>\n")
      (body (args))))
    (term_1
     ((body (args))
      (see (ref ((in-module library!references) (name v:my_term)))
       (label references.my_term) (span references.lua[17:11-17:28]))
      (see (ref ((in-module library!references) (name v:my_term)))
       (label my_term) (span references.lua[16:12-16:18]))))
    (term_2
     ((description
       "<p>See [<code>my_term</code>][((in-module library!references) (name v:my_term))], [<code>references.my_term</code>][((in-module library!references) (name v:my_term))] and [custom label][((in-module library!references) (name v:my_term))].</p>\n")
      (body (args))))
    (type_1
     ((body (args))
      (see (ref ((in-module library!references) (name v:my_term)))
       (label references.my_term) (span references.lua[25:11-25:36]))
      (see (ref ((in-module library!references) (name v:my_term)))
       (label references.my_term) (span references.lua[24:11-24:28]))
      (see (ref ((in-module library!references) (name v:my_term)))
       (label my_term) (span references.lua[23:12-23:18]))))
    (type_2
     ((description
       "<p>See [<code>MyType</code>][((in-module library!references) (name ty:MyType))] and [<code>references.MyType</code>][((in-module library!references) (name ty:MyType))]</p>\n")
      (body (args))))
    (type_3
     ((description
       "<p>See [<code>MyType</code>][((in-module library!references) (name ty:MyType))], [<code>references.MyType</code>][((in-module library!references) (name ty:MyType))] and [<code>references.MyType</code>][((in-module library!references) (name ty:MyType))]</p>\n")
      (body
       (returns
        (return (type ((in-module library!references) (name ty:MyType))))))))
    (method_1
     ((description
       "<p>See [<code>MyType:meth</code>][((in-module library!references) (name ty:MyType:meth))] and [<code>MyType.meth</code>][((in-module library!references) (name ty:MyType:meth))]</p>\n")
      (body (args))))
    (builtin_1
     ((body (args))
      (see (ref ((name number))) (label number)
       (span references.lua[40:11-40:16]))
      (see
       (ref
        ((name string) (url https://www.lua.org/manual/5.1/manual.html#5.4)))
       (label string) (span references.lua[39:11-39:16]))
      (see
       (ref
        ((name string.match)
         (url https://www.lua.org/manual/5.1/manual.html#pdf-string.match)))
       (label string.match) (span references.lua[38:12-38:23]))))
    (builtin_2
     ((description
       "<p>[<code>string.match</code>][((name string.match) (url https://www.lua.org/manual/5.1/manual.html#pdf-string.match))], [<code>string</code>][((name string) (url https://www.lua.org/manual/5.1/manual.html#5.4))] and [<code>number</code>][((name number))]</p>\n")
      (body (args))))
    (module_1
     ((description
       "<p>[<code>references</code>][((in-module library!references))]</p>\n")
      (body (args))))))
  (type
   (body (name MyType)
    (member (name meth) (method) (value ((body ((args) (self))))))))))