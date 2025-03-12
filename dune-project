(lang dune 3.17)
(using menhir 2.1)
(implicit_transitive_deps false)
(generate_opam_files true)

(name illuaminate)
(version 0.1.0)
(authors "Jonathan Coates")
(license BSD-3-Clause)
(source (github squiddev/illuaminate))
(maintainers "Jonathan Coates <git@squiddev.cc>")

(package
  (name illuaminate)
  (synopsis "Source code analysis for Lua")
  (description "Provides various tools for parsing, processing and producing Lua source code. Currently does very little, but will hopefully grow in the future.")
  (depends
    (ocaml (>= 5.3))

    ;; Build-time
    (js_of_ocaml-compiler (and :build (>= 4.0.0)))
    (js_of_ocaml-ppx :build)
    (ppx_deriving :build)
    (ppxlib (and :build (>= 0.14.0)))
    (reason :build)

    ; Test
    (alcotest :with-test)
    (omnomnom :with-test)
    (qcheck-core :with-test)
    (qcheck-alcotest :with-test)

    ; Documentation generation
    (odoc :with-doc)

    ; Dev-only dependencies.
    (bisect_ppx (and :with-test (>= 2.5.0)))
    (ocamlformat :dev)

    ; Core
    cmdliner
    (containers (>= 3.6.0))
    fmt
    fpath
    js_of_ocaml
    logs
    lrgrep
    markup
    menhir
    menhirLib
    re
    uri
    yojson))
