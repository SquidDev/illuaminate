## Colours

Illuaminate allows using hex colour codes in inline text. These will be expanded
to show a preview of the colour.

```````````````````````````````` example
#4C4C4C
.
<p><span class="color-ref" style="background-color: #4C4C4C"></span><span class="color">#4C4C4C</span></p>
````````````````````````````````

```````````````````````````````` example
#4C4
.
<p><span class="color-ref" style="background-color: #4C4"></span><span class="color">#4C4</span></p>
````````````````````````````````

Other formats (for instance a space between the `#` and colour, non-hex colours or not 3/6 characters long) are not
accepted.

```````````````````````````````` example
( # 4C4 )
.
<p>( # 4C4 )</p>
````````````````````````````````

```````````````````````````````` example
( #2az )
.
<p>( #2az )</p>
````````````````````````````````

```````````````````````````````` example
( #2abcdef )
.
<p>( #2abcdef )</p>
````````````````````````````````

Some other interested cases worth handling:


```````````````````````````````` example
( #2abcde, #2abcde. ) 
.
<p>( <span class="color-ref" style="background-color: #2abcde"></span><span class="color">#2abcde</span>, <span class="color-ref" style="background-color: #2abcde"></span><span class="color">#2abcde</span>. )</p>
````````````````````````````````

## References

Illuaminate also provides syntax to reference symbols defined elsewhere in the
code. These are different to Markdown's standard link syntax.

One can either write the symbol directly, in which case it is formatted as code:

```````````````````````````````` example
@{print}
.
<p><span class="reference reference-code reference-unresolved">print</span></p>
````````````````````````````````

or with an additional label, in which case it is formatted as text.

```````````````````````````````` example
@{print|print a string}
.
<p><span class="reference reference-text reference-unresolved">print a string</span></p>
````````````````````````````````

The label _should_ accept arbitrary markdown, but currently do not.

```````````````````````````````` example
@{print|**print a string**}
.
<p><span class="reference reference-text reference-unresolved">**print a string**</span></p>
````````````````````````````````

Needless to say, unclosed blocks are treated as normal text:

```````````````````````````````` example
@{print|print a string
.
<p>@{print|print a string</p>
````````````````````````````````

## Admonitions
Illuaminate supports Admonitions, inspired by those from [Docusaurus](https://docusaurus.io/docs/markdown-features/admonitions)
and [remark-admonitions](https://github.com/elviswolcott/remark-admonitions).

These are wrapped by exactly three colons, with another block nested inside.

```````````````````````````````` example
:::warning
Some content
:::
.
<div class="admonition admonition-warning"><h5 class="admonition-heading">warning</h5>
<p>Some content</p>
</div>
````````````````````````````````

These admonitions can contain standard Markdown, including blocks and inline elements

```````````````````````````````` example
:::warning
For instance, *we can have emphasis*

 - Multiple paragraphs
 - And lists
:::
.
<div class="admonition admonition-warning"><h5 class="admonition-heading">warning</h5>
<p>For instance, <em>we can have emphasis</em></p>
<ul>
<li>Multiple paragraphs</li>
<li>And lists</li>
</ul>
</div>
````````````````````````````````

Like any other block, these are closed automatically when their context is terminated

```````````````````````````````` example
> :::warning
> Closed by our blockquote

 - :::warning
   And by
 - another list item
.
<blockquote>
<div class="admonition admonition-warning"><h5 class="admonition-heading">warning</h5>
<p>Closed by our blockquote</p>
</div>
</blockquote>
<ul>
<li><div class="admonition admonition-warning"><h5 class="admonition-heading">warning</h5>
<p>And by</p>
</div></li>
<li>another list item</li>
</ul>
````````````````````````````````

This does mean that ::: needs to be demoted to a normal string.

```````````````````````````````` example
> :::warning
> Closed by our blockquote

:::
.
<blockquote>
<div class="admonition admonition-warning"><h5 class="admonition-heading">warning</h5>
<p>Closed by our blockquote</p>
</div>
</blockquote>
<p>:::</p>
````````````````````````````````

We also allow custom titles.

```````````````````````````````` example
:::warning *Custom title!*
Some content
:::
.
<div class="admonition admonition-warning"><h5 class="admonition-heading"><em>Custom title!</em></h5>
<p>Some content</p>
</div>
````````````````````````````````
