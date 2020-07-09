((body (name references)
  (contents
   (table (my_term ((body table)))
    (unknown_1
     ((body (args))
      (see (ref unknown) (label unknown) (span references.lua[10:12-10:18]))))
    (unknown_2
     ((description
       "See <illuaminate:ref link='unknown' style='code'>unknown</illuaminate:ref> and <illuaminate:ref link='unknown' style='text'>custom label</illuaminate:ref>.\n\n")
      (body (args))))
    (term_1
     ((body (args))
      (see (ref ((in-module references) (name v:my_term)))
       (label references.my_term) (span references.lua[17:11-17:28]))
      (see (ref ((in-module references) (name v:my_term))) (label my_term)
       (span references.lua[16:12-16:18]))))
    (term_2
     ((description
       "See <illuaminate:ref module='references' sec='v:my_term' style='code'>my_term</illuaminate:ref>, <illuaminate:ref module='references' sec='v:my_term' style='code'>references.my_term</illuaminate:ref> and <illuaminate:ref module='references' sec='v:my_term' style='text'>custom label</illuaminate:ref>.\n\n")
      (body (args))))
    (type_1
     ((body (args))
      (see (ref ((in-module references) (name v:my_term)))
       (label references.my_term) (span references.lua[24:11-24:28]))
      (see (ref ((in-module references) (name v:my_term))) (label my_term)
       (span references.lua[23:12-23:18]))))
    (type_2
     ((description
       "See <illuaminate:ref module='references' sec='ty:MyType' style='code'>MyType</illuaminate:ref> and <illuaminate:ref module='references' sec='ty:MyType' style='code'>references.MyType</illuaminate:ref>\n\n")
      (body (args))))
    (type_3
     ((description
       "See <illuaminate:ref module='references' sec='ty:MyType' style='code'>MyType</illuaminate:ref> and <illuaminate:ref module='references' sec='ty:MyType' style='code'>references.MyType</illuaminate:ref>\n\n")
      (body
       (returns (return (type ((in-module references) (name ty:MyType))))))))
    (method_1
     ((description
       "See <illuaminate:ref module='references' sec='ty:MyType:meth' style='code'>MyType:meth</illuaminate:ref> and <illuaminate:ref module='references' sec='ty:MyType:meth' style='code'>MyType.meth</illuaminate:ref>\n\n")
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
         (url https://www.lua.org/manual/5.1/manual.html#string.match)))
       (label string.match) (span references.lua[37:12-37:23]))))
    (builtin_2
     ((description
       "<illuaminate:ref href='https://www.lua.org/manual/5.1/manual.html#string.match' style='code'>string.match</illuaminate:ref>, <illuaminate:ref href='https://www.lua.org/manual/5.1/manual.html#5.4' style='code'>string</illuaminate:ref> and <illuaminate:ref style='code'>number</illuaminate:ref>\n\n")
      (body (args))))
    (module_1
     ((description
       "<illuaminate:ref module='references' style='code'>references</illuaminate:ref>\n\n")
      (body (args))))))
  (type
   (body (name MyType)
    (member (name meth) (method) (value ((body ((args) (self))))))))))