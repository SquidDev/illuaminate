(lang dune 2.3)
(using menhir 2.0)
(implicit_transitive_deps false)
(generate_opam_files true)
(allow_approximate_merlin)

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
    (ocaml (>= 4.08))

    ;; Build-time
    (dune (>= 2.3)) ; While a build-tool, dune shouldn't be build-time only
    (js_of_ocaml-compiler (and :build (>= 3.6.0)))
    (js_of_ocaml-ppx :build)
    (ppx_deriving :build)
    (ppxlib (and :build (>= 0.14.0)))
    (reason :build)

    ; Test
    (alcotest :with-test)
    (junit :with-test)
    (qcheck-core :with-test)
    (omnomnom :with-test)

    ; Documentation generation
    (odoc :with-doc)

    ; Dev-only dependencies.
    (bisect_ppx :dev)
    (ocamlformat :dev)
    (opam-file-format :dev)

    ; Core
    cmdliner
    containers
    fpath
    js_of_ocaml
    logs
    menhir
    omd
    ppx_deriving_yojson
    re
    result
    yojson

    ;; Language server
    lsp))
