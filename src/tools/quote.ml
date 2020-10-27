(** Reads a file and converts it into an ML module containing that file. *)

let () =
  match Sys.argv with
  | [| _; input |] -> CCIO.File.read_exn input |> Format.printf "let contents = %S\n%!"
  | _ ->
      Format.eprintf "%s FILE\n%!" Sys.executable_name;
      exit 1
