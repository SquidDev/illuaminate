((body (id foo) (title foo)
  (contents
   (table
    (a
     ((description "<p>A basic variable</p>\n")
      (body expr (ty ((name number))) (value 1))
      (example
       "<pre><code class=\"language-lua\">local bar = require &quot;foo&quot;\n</code></pre>\n")
      (example
       "<pre><code class=\"language-lua\">local foo = require &quot;foo&quot;\nprint(foo.a)\n</code></pre>\n")))))))