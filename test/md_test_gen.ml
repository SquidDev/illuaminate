(** Generate our [rules.inc] file. *)

let () =
  let files = ref [] in
  let extra = ref false in
  Arg.parse
    [ ("--extra", Arg.Set extra, "Add a dependency on files in the extra/ directory.") ]
    (fun x -> files := Filename.remove_extension x :: !files)
    __MODULE__;
  let files = List.sort String.compare !files and extra = !extra in
  List.iter
    (fun name ->
      let println x = Printf.printf (x ^^ "\n") in
      println
        {|(rule %s(action (with-stdout-to %s.new (with-stdin-from %%{dep:%s.md} (run %%{dep:./main.exe} md)))))|}
        (if extra then "(deps (glob_files extra/*.lua)) " else "")
        name name;
      println {|(rule (alias %s) (action (diff %s.md %s.new)))|} name name name)
    files;
  print_string {|
(alias
 (name runtest)
 (deps|};
  List.iter (Printf.printf "\n  (alias %s)") files;
  print_endline "))"
