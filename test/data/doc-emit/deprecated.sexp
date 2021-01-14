((description "A module demonstrating deprecated terms.\n\n")
 (body (id deprecation) (title deprecation)
  (contents
   (table (a ((body expr (ty ((name number))) (value 1)) (deprecated)))
    (b
     ((body expr (ty ((name number))) (value 2))
      (deprecated "Please don't use this!\n\n"))))))
 (deprecated "Don't use this\n\n"))