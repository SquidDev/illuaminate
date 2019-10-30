(lang dune 1.10)
(using menhir 2.0)
(using fmt 1.1 (enabled_for ocaml dune))
(implicit_transitive_deps false)

(generate_opam_files true)
(authors SquidDev)
(license BSD-3-Clause)
(source (github squiddev/lsp))

(package
  (name illuaminate)
  (synopsis "Source code analysis for Lua.")
  (description "Provides various tools for parsing, processing and producing Lua source code. Currently does very little, but will hopefully grow in the future.")
  (depends
    (ocaml (>= 4.06))
    ; Build-time (:build)
    (dune (and :build (>= 1.10)))
    (ppxlib :build)
    (ppx_deriving :build)
    (reason :build)
    (odoc :build)
    ; Test (:with-test)
    (omnomnom :with-test)
    ; Runtime
    omd
    cmdliner
    menhir
    yojson
    base-unix
    containers))
