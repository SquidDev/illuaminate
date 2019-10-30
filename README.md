# 💡 Illuaminate
Source code analysis for Lua

Illuaminate is a series of tools for working with Lua source code, providing
ways of making your codebase clearer and easier to understand. It's currently
rather bare bones, and still at a POC stage, but hopefully will expand and
become more useful as it continues to develop.

## Features
 - Many basic static analysis features (dead code detection, unused variables, etc...).
 - Automatic code fixer for a some detected errors.

## Building
 - Install [opam][] and set up a OCaml 4.08.1 switch (`opam switch create
   4.08.1`). Later versions may work, but haven't been tested.
 - Download [omnomnom][], and install it using `git clone https://github.com/SquidDev/omnomnom.git && cd omnomnom && opam install ./omnomnom.opam`.
 - Install all other dependencies: `opam install --deps-only ./illuaminate.opam`
 - Run illuaminate: `opam exec -- illuaminate`


## Example
```
> illuaminate lint bios.lua -- From CC-Tweaked's bios
bios.lua:[770:40-770:62]: Unnecessary parenthesis. [syntax:redundant-parens]
     │
770  │                 table.insert( tResults, (bIncludeDirs and ".") or "./" )
     │                                         ^^^^^^^^^^^^^^^^^^^^^^
bios.lua:[828:8-828:9]: Unused variable "n". [var:unused]
     │
828  │     for n,sFile in ipairs( tApis ) do
     │
bios.lua:[863:8-863:12]: Setting unknown global variable "exec". [var:set-global]
     │
863  │         exec = commands.exec
     │         ^^^^
```

[opam]: https://opam.ocaml.org/doc/Install.html
[omnomnom]: https://github.com/SquidDev/omnomnom
