---
module: text_page
---

<!-- Comment before the header -->

# An example module!
This is a small example module. Its main purpose is testing doc generation for
Markdown files.

# Images
Images can be included with Markdown:

![](example.svg)

and as HTML:

<img src="example.svg" />

URLs, absolute files, and missing files are not included

![](https://example.com/example.svg) ![](/example.svg) ![](missing.svg)
