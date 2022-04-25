(** Reads a file and converts it into an ML module containing that file. *)

let () =
  let args = Sys.argv in
  if Array.length args <= 1 then (
    Format.eprintf "%s FILE\n%!" Sys.executable_name;
    exit 1);

  let out = Buffer.create 32 in
  for i = 1 to Array.length args - 1 do
    CCIO.File.read_exn args.(i) |> Buffer.add_string out
  done;

  Buffer.contents out |> Format.printf "let contents = %S\n%!"
