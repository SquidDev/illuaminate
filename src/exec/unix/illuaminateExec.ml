let rec waitpid_non_intr pid =
  try Unix.waitpid [] pid |> snd with Unix.Unix_error (EINTR, _, _) -> waitpid_non_intr pid

let capture cmd args =
  let out_read, out_write = Unix.pipe ~cloexec:true () in
  try
    let outchan = Unix.in_channel_of_descr out_read in
    let pid = Unix.create_process cmd args Unix.stdin out_write Unix.stderr in
    Unix.close out_write;
    let result = waitpid_non_intr pid in
    let contents = CCIO.read_all outchan in
    Ok (contents, result)
  with Unix.Unix_error (err, _, _) ->
    Unix.close out_read;
    Unix.close out_write;
    Unix.error_message err |> Printf.sprintf "%s exited with error '%s'" cmd |> Result.error

let check_status cmd = function
  | Unix.WEXITED 0 -> Ok ()
  | WEXITED 127 -> Printf.sprintf "%s is not installed" cmd |> Result.error
  | WEXITED x -> Printf.sprintf "%s exited with %d" cmd x |> Result.error
  | WSIGNALED x -> Printf.sprintf "%s was signaled with code %d" cmd x |> Result.error
  | WSTOPPED x -> Printf.sprintf "%s was stopped with %d" cmd x |> Result.error

let exec cmd args =
  capture cmd args
  |> CCResult.flat_map @@ fun (line, result) ->
     check_status cmd result |> Result.map (fun () -> line)
