((description
  "<p>This is a simple module which contains a function</p>\n<h1>References:</h1>\n<p>No label: [<code>some_value</code>][((in-module library!x) (name v:some_value))] [<code>some_value</code>][((in-module library!x) (name v:some_value))]</p>\n<p>With label [label][((in-module library!x) (name v:some_value))] [label][((in-module library!x) (name v:some_value))]</p>\n<p>External modules: [module path][((name package.path) (url https://www.lua.org/manual/5.1/manual.html#pdf-package.path))] [module path][((name package.path) (url https://www.lua.org/manual/5.1/manual.html#pdf-package.path))]</p>\n<p>Across multiple lines: [a multiline\nlabel][((in-module library!x) (name v:some_value))]. [a multiline\nlabel][((in-module library!x) (name v:some_value))]</p>\n<h1>Admonitions</h1>\n<h2>Old syntax</h2>\n<div class=\"admonition admonition-danger\"><h5 class=\"admonition-heading\">danger</h5><p>Some warning</p>\n</div><div class=\"admonition admonition-danger\"><h5 class=\"admonition-heading\">With a label</h5><p>Some warning</p>\n</div><h2>New syntax</h2>\n<blockquote>\n<p>!WARNING<!-- Undefined label !warning -->\nCritical content demanding immediate user attention due to potential risks.</p>\n</blockquote>\n<blockquote>\n<p>With a label<!-- Undefined label !warning -->\nCritical content demanding immediate user attention due to potential risks.</p>\n</blockquote>\n<h1>Colours</h1>\n<p><span class=\"color-ref\" style=\"background-color: #fff\"></span><span class=\"color\">#fff</span> <span class=\"color-ref\" style=\"background-color: #fda087\"></span><span class=\"color\">#fda087</span></p>\n<h1>Attributes on code blocks</h1>\n<pre><code class=\"language-lua\">print(&quot;Hello&quot;)\n</code></pre>\n<pre><code class=\"language-python\">print(&quot;Hello&quot;)\n</code></pre>\n<h1>Attributes on headers</h1>\n<h2 id=\"foo\"><a class=\"anchor\" aria-hidden=\"true\" href=\"#foo\"></a>Some header</h2>\n")
 (body (id x)
  (contents
   (table (some_value ((body expr (ty ((name number))) (value 0))))))))