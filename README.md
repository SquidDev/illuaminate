# 💡 Illuaminate
Source code analysis for Lua

Illuaminate is a series of tools for working with Lua source code, providing
ways of making your codebase clearer and easier to understand. It's currently
rather bare bones, and still at a POC stage, but hopefully will expand and
become more useful as it continues to develop.

<p align="center">
<img src="doc/example.png" />
</p>

## Features
 - Many basic static analysis features (dead code detection, unused variables,
   etc...) as well as several formatting linters (whitespace, redundant
   parenthesis, etc...).
 - Automatic code fixer for many of the detected problems.
 - Integration with GitHub Actions

## Building
 - Install [opam][] and set up a OCaml 4.08.1 switch (`opam switch create
   4.08.1`). Later versions may work, but haven't been tested.
 - Insall [omnomnom][] using `opam pin add omnomnom https://github.com/SquidDev/omnomnom.git`.
 - Install all other dependencies: `opam install --deps-only ./illuaminate.opam`
 - Build using Dune: `dune build && dune runtest`
 - You may either install illuaminate into opam using `dune install`, or use the
   files in the `_build/install/default/bin` directory.

## Usage
 - Either build Illuaminate as above, or download a pre-built version:
   ```
   > wget https://squiddev.cc/illuaminate/bin/illuaminate # A x86-64 bit Linux static binary
   > wget https://squiddev.cc/illuaminate/bin/illuaminate.js # Cross-platform JavaScript file which can be run with Node
   ```

   You may need to `chmod +x` the downloaded file to make it executable.

 - Run `illuaminate lint` to lint the current directory, or `illuaminate lint
   my/sub/directory` to lint a specific file or folder. You should receive a report of
   all problems detected.

 - Use `illuaminate fix` (or `illuaminate fix my/sub/directory`) to attempt to
   fix any detected problems. This command modifies the file in place, so it's a
   good idea to check your files into version control before fixing, and make
   sure you're happy with the changes made.

 - You can get more fine-grained control over illuaminate using its
   configuration files. `illuamiante init-config illuaminate.sexp` will generate
   a template config file which can be adjusted to suit your needs.

### GitHub actions
While illuaminate can be used from any CI system, we have somewhat extended
support for GitHub actions. Running `illuaminate lint --github` will upload any
warnings as annotations, allowing them to be viewed within the GitHub
interfaces:

<p align="center">
<img src="doc/gh-annotations.png" />
</p>

This can be done by adding the following to your workflow file:

```yml
# name, on, etc...

jobs:
  build:
    runs-on: ubuntu-latest

    steps:
      # ...
    - name: Lint Lua code
      run: |
        # Download illuaminate
        test -d bin || mkdir bin
        test -f bin/illuaminate || wget -q -Obin/illuaminate https://squiddev.cc/illuaminate/bin/illuaminate
        chmod +x bin/illuaminate
        # And run it
        GITHUB_TOKEN=${{ secrets.GITHUB_TOKEN }} bin/illuaminate lint --github
```


[opam]: https://opam.ocaml.org/doc/Install.html
[omnomnom]: https://github.com/SquidDev/omnomnom
[actions]: https://github.com/features/actions
