(lang dune 2.0)
(using menhir 2.0)
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
  (synopsis "Source code analysis for Lua.")
  (description "Provides various tools for parsing, processing and producing Lua source code. Currently does very little, but will hopefully grow in the future.")
  (depends
    (ocaml (>= 4.06))
    ; Build-time (:build)
    (dune (and :build (>= 2.0)))
    (ppxlib :build)
    (ppx_deriving :build)
    (odoc :build)
    ; Test (:with-test)
    (omnomnom :with-test)
    (patience_diff :with-test)
    (junit :with-test)
    ; Runtime
    cmdliner
    containers
    menhir
    omd
    re))
