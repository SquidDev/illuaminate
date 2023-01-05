(lang dune 3.1)
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
    (ocaml (and (>= 4.14) (< 5.0)))

    ;; Build-time
    (dune (>= 3.1))
    (js_of_ocaml-compiler (and :build (>= 4.0.0)))
    (js_of_ocaml-ppx :build)
    (ppx_deriving :build)
    (ppxlib (and :build (>= 0.14.0)))
    (reason :build)

    ; Test
    (alcotest :with-test)
    (fmt :with-test)
    (junit :with-test)
    (omnomnom :with-test)
    (qcheck-core :with-test)

    ; Documentation generation
    (odoc :with-doc)

    ; Dev-only dependencies.
    (bisect_ppx (and :with-test (>= 2.5.0)))
    (ocamlformat :dev)
    (opam-file-format (and :dev (>= 2.1.0)))

    ; Core
    cmdliner
    (containers (>= 3.6.0))
    fpath
    js_of_ocaml
    logs
    menhir
    menhirLib
    (mtime (< 2.0))
    ppx_deriving_yojson
    ppx_yojson_conv_lib
    re
    result
    yojson

    ;; Language server
    (lsp (= 1.5.0))
    jsonrpc))
