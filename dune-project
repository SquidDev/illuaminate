(lang dune 2.3)
(using menhir 2.0)
(using library_variants 0.2)
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
    (ocaml (>= 4.08))
    ; Build-time (:build)
    (dune (>= 2.0))
    (ppxlib :build)
    (reason :build)
    (ppx_deriving :build)
    (js_of_ocaml-ppx :build)
    (js_of_ocaml-compiler :build)
    (odoc :with-doc)
    ; Test (:with-test)
    (omnomnom :with-test)
    (patience_diff :with-test)
    (junit :with-test)
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
    ; GitHub integration (HTTP support mostly)
    ; We disable this on Windows for now, as it's not easy to build the an SSL library.
    (lwt (<> :os-family windows))
    (cohttp-lwt-unix (<> :os-family windows))
    (tls (<> :os-family windows))
    ))
