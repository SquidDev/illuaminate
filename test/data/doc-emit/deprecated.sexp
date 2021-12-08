((description "<p>A module demonstrating deprecated terms.</p>\n")
 (body (id deprecation)
  (contents
   (table (a ((body expr (ty ((name number))) (value 1)) (deprecated)))
    (b
     ((body expr (ty ((name number))) (value 2))
      (deprecated "<p>Please don't use this!</p>\n"))))))
 (deprecated "<p>Don't use this</p>\n"))