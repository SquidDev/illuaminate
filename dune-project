(lang dune 2.7)
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
    (ocaml (and (>= 4.08) (< 4.13)))

    ;; Build-time
    dune ; While a build-tool, dune shouldn't be build-time only
    (js_of_ocaml-compiler (and :build (>= 3.6.0)))
    (js_of_ocaml-ppx :build)
    (ppx_deriving :build)
    (ppxlib (and :build (>= 0.14.0)))
    (reason :build)

    ; Test
    (alcotest :with-test)
    (junit :with-test)
    (qcheck-core :with-test)
    (fmt :with-test)

    ; Documentation generation
    (odoc :with-doc)

    ; Dev-only dependencies.
    (bisect_ppx (and :dev (>= 2.5.0)))
    (ocamlformat :dev)
    (opam-file-format (and :dev (>= 2.1.0)))

    ; Core
    cmdliner
    (containers (>= 3.0.1))
    fpath
    js_of_ocaml
    logs
    menhir
    menhirLib
    mtime
    (omd (= 1.3.1))
    ppx_deriving_yojson
    ppx_yojson_conv_lib
    re
    result
    yojson

    ;; Language server
    (lsp (= 1.5.0))
    jsonrpc))
