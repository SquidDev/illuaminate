open B0_kit.V000
open Result.Syntax

let commonmark_version =
  (* If you update this, also update Cmarkit.commonmark_version
     and the links in src/*.mli *)
  "0.30"

(* OCaml library names *)

let cmarkit = B0_ocaml.libname "cmarkit"
let cmdliner = B0_ocaml.libname "cmdliner"
let uucp = B0_ocaml.libname "uucp"

let b0_std = B0_ocaml.libname "b0.std"
let b0_b00_kit = B0_ocaml.libname "b0.b00.kit"

(* Libraries *)

let cmarkit_lib =
  let srcs = Fpath.[ `Dir (v "src") ] in
  let requires = [] and name = "cmarkit-lib" in
  B0_ocaml.lib cmarkit ~name ~doc:"The cmarkit library" ~srcs ~requires

(* Tools *)

let cmarkit_tool =
  let srcs = Fpath.[`Dir (v "tool")] in
  let requires = [cmarkit; cmdliner] in
  B0_ocaml.exe "cmarkit" ~doc:"The cmarkit tool" ~srcs ~requires

(* Unicode support *)

let unicode_data =
  let srcs = Fpath.[`File (v "support/unicode_data.ml")] in
  let requires = [uucp] in
  let doc = "Generate cmarkit Unicode data" in
  B0_ocaml.exe "unicode_data" ~doc ~srcs ~requires

let update_unicode =
  B0_cmdlet.v "update_unicode_data" ~doc:"Update Unicode character data" @@
  fun env _args -> B0_cmdlet.exit_of_result @@
  (* FIXME b0 *)
  let b0 = Os.Cmd.get_tool (Fpath.v "b0") |> Result.get_ok in
  let unicode_data = Cmd.(path b0 % "--" % "unicode_data") in
  let outf = B0_cmdlet.in_scope_dir env (Fpath.v "src/cmarkit_data_uchar.ml") in
  let outf = Os.Cmd.out_file ~force:true ~make_path:false outf in
  Os.Cmd.run ~stdout:outf unicode_data

(* Tests *)

let update_spec_tests =
  B0_cmdlet.v "update_spec_tests" ~doc:"Update the CommonMark spec tests" @@
  fun env _args -> B0_cmdlet.exit_of_result @@
  let tests =
    Fmt.str "https://spec.commonmark.org/%s/spec.json" commonmark_version
  in
  let dest = B0_cmdlet.in_scope_dir env (Fpath.v ("test/spec.json")) in
  let dest = Os.Cmd.out_file ~force:true ~make_path:false dest in
  let* curl = Os.Cmd.get Cmd.(atom "curl" % "-L" % tests) in
  Os.Cmd.run ~stdout:dest curl

let spec_srcs = Fpath.[`File (v "test/spec.ml"); `File (v "test/spec.mli")]

let bench =
  let srcs = Fpath.[`File (v "test/bench.ml")] in
  let requires = [cmarkit] in
  let meta = B0_meta.(empty |> tag bench) in
  let doc = "Simple standard CommonMark to HTML renderer for benchmarking" in
  B0_ocaml.exe "bench" ~doc ~meta ~srcs ~requires

let test_spec =
  let srcs = Fpath.(`File (v "test/test_spec.ml") :: spec_srcs) in
  let requires = [ b0_std; b0_b00_kit; cmarkit ] in
  let meta =
    B0_meta.(empty |> add B0_unit.Action.exec_cwd B0_unit.Action.scope_cwd)
  in
  let doc = "Test CommonMark specification conformance tests" in
  B0_ocaml.exe "test_spec" ~doc ~meta ~srcs ~requires

let trip_spec =
  let srcs = Fpath.(`File (v "test/trip_spec.ml") :: spec_srcs) in
  let requires = [ b0_std; b0_b00_kit; cmarkit ] in
  let meta =
    B0_meta.(empty |> add B0_unit.Action.exec_cwd B0_unit.Action.scope_cwd)
  in
  let doc = "Test CommonMark renderer on conformance tests" in
  B0_ocaml.exe "trip_spec" ~doc ~meta ~srcs ~requires

let pathological =
  let srcs = Fpath.[`File (v "test/pathological.ml")] in
  let requires = [ b0_std ] in
  let doc = "Test a CommonMark parser on pathological tests." in
  B0_ocaml.exe "pathological" ~doc ~srcs ~requires

let examples =
  let srcs = Fpath.[`File (v "test/examples.ml")] in
  let requires = [cmarkit] in
  let meta = B0_meta.(empty |> tag test) in
  let doc = "Doc sample code" in
  B0_ocaml.exe "examples" ~doc ~meta ~srcs ~requires

let expect_trip_spec exp env =
  (* FIXME b0 *)
  let trip_spec = Cmd.(atom "b0" % "--path" % "--" % "trip_spec") in
  let* trip_spec = Result.map Cmd.atom (Os.Cmd.run_out ~trim:true trip_spec) in
  let stdout = Fpath.(B0_expect.base exp / "spec.trip") in
  let cwd = B0_cmdlet.Env.scope_dir env in
  B0_expect.stdout exp ~cwd ~stdout trip_spec

let expect_test =
  (* FIXME B0_expect, There's still a bit to streamline here *)
  let doc = "Test the expectations" in
  B0_cmdlet.v "test_expect" ~doc @@ fun env args ->
  B0_cmdlet.exit_of_result' @@
  (* FIXME b0 *)
  let trip = Cmd.(atom "b0" % "--path" % "--" % "cmarkit") in
  let* trip = Result.map Cmd.atom (Os.Cmd.run_out ~trim:true trip) in
  let* exp = B0_expect.make env ~base:Fpath.(v "test/expect") in
  let* base_files = B0_expect.base_files exp ~recurse:false in
  let is_input f = Fpath.has_ext ".md" f && not (Fpath.has_ext ".trip.md" f) in
  let test_files = List.filter is_input base_files in
  let render_tests file acc =
    let run_test (cmd, ext) acc =
      let cwd = B0_expect.base exp and stdout = Fpath.(file -+ ext) in
      let base = Fpath.basename file in
      let with_exts = String.ends_with ~suffix:"exts.md" base in
      let inf = Fpath.strip_prefix cwd file |> Option.get in
      let cmd = Cmd.(cmd %% if' with_exts (atom "--exts") %% path inf) in
      let* o = B0_expect.stdout exp ~cwd ~stdout Cmd.(trip %% cmd) in
      Ok (o :: acc)
    in
    let renderers =
      [ Cmd.(atom "html" % "-c" % "--unsafe"), ".html";
        Cmd.(atom "latex"), ".latex";
        Cmd.(atom "commonmark"), ".trip.md";
        Cmd.(atom "locs"), ".locs";
        Cmd.(atom "locs" % "--no-layout"), ".nolayout.locs"; ]
    in
    List.fold_stop_on_error run_test renderers acc
  in
  let* os = List.fold_stop_on_error render_tests test_files [] in
  let* o = expect_trip_spec exp env in
  Ok (B0_expect.log_results exp (o :: os))

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The cmarkit programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/cmarkit"
    |> add online_doc "https://erratique.ch/software/cmarkit/doc"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/cmarkit.git"
    |> add issues "https://github.com/dbuenzli/cmarkit/issues"
    |> add description_tags
      ["codec"; "commonmark"; "markdown"; "org:erratique"; ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
                  "--with-cmdliner" "%{cmdliner:installed}%"]]|}
    |> tag B0_opam.tag
    |> add B0_opam.Meta.depopts ["cmdliner", ""]
    |> add B0_opam.Meta.conflicts [ "cmdliner", {|< "1.1.0"|}]
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "4.14.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|};
        "uucp", {|dev|};
        "b0", {|dev & with-test|};
      ]
  in
  B0_pack.v "default" ~doc:"cmarkit package" ~meta ~locked:true @@
  B0_unit.list ()
