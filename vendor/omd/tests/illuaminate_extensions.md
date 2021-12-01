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
