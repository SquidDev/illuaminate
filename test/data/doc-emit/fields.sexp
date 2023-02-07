((body (id fields) (contents (table))
  (type (description "<p>A table with some fields</p>\n")
   (body (name A)
    (member (name b)
     (value ((description "<p>Field b.</p>\n") (body unknown))))
    (member (name a)
     (value
      ((description "<p>Field a.</p>\n")
       (body (expr (ty (union ((name number)) nil)))))))))))