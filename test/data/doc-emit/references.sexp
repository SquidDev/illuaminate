((body (name references)
  (contents
   (table (my_term ((body table)))
    (unknown_1 ((body (args)) (see (ref unknown) (label unknown))))
    (unknown_2
     ((description
       "See <illuaminate:ref link='unknown'>unknown</illuaminate:ref>\n\n")
      (body (args))))
    (term_1
     ((body (args))
      (see (ref ((in-module references) (name v:my_term)))
       (label references.my_term))
      (see (ref ((in-module references) (name v:my_term))) (label my_term))))
    (term_2
     ((description
       "See <illuaminate:ref module='references' sec='v:my_term'>my_term</illuaminate:ref> and <illuaminate:ref module='references' sec='v:my_term'>references.my_term</illuaminate:ref>\n\n")
      (body (args))))
    (type_1
     ((body (args))
      (see (ref ((in-module references) (name v:my_term)))
       (label references.my_term))
      (see (ref ((in-module references) (name v:my_term))) (label my_term))))
    (type_2
     ((description
       "See <illuaminate:ref module='references' sec='ty:MyType'>MyType</illuaminate:ref> and <illuaminate:ref module='references' sec='ty:MyType'>references.MyType</illuaminate:ref>\n\n")
      (body (args))))
    (type_3
     ((description
       "See <illuaminate:ref module='references' sec='ty:MyType'>MyType</illuaminate:ref> and <illuaminate:ref module='references' sec='ty:MyType'>references.MyType</illuaminate:ref>\n\n")
      (body
       (returns
        ((type ((in-module references) (name ty:MyType))) (description ))))))
    (method_1
     ((description
       "See <illuaminate:ref module='references' sec='ty:MyType:meth'>MyType:meth</illuaminate:ref> and <illuaminate:ref link='MyType.meth'>MyType.meth</illuaminate:ref>\n\n")
      (body (args))))))
  (type
   (body (name MyType)
    (member (name meth) (method) (value ((body ((args) (self))))))))))