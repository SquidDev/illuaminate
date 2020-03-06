open IlluaminateCore
open CCResult.Infix

let src = Logs.Src.create ~doc:"Makes requests to the GH actions API" __MODULE__

module Log = (val Logs.src_log src)

let root = "https://api.github.com"

let read_and_delete path =
  let result =
    match open_in path with
    | channel ->
        let buffer = Buffer.create 1024 in
        let bytes = Bytes.create 8192 in
        let rec read () =
          let n = input channel bytes 0 8192 in
          Buffer.add_subbytes buffer bytes 0 n;
          if n > 0 then read ()
        in
        let () = read () in
        close_in channel;
        Some (Buffer.contents buffer)
    | exception e ->
        Log.err (fun f -> f "Cannot read %s (%s)" path (Printexc.to_string e));
        None
  in
  if Sys.file_exists path then Sys.remove path;
  result

(** Make a request against the GitHub API. *)
let request ~token ~path ~mthd ~body : (Yojson.Safe.t, string) result =
  let body = Yojson.Safe.to_string ~std:true body in
  let tmpfile = Filename.temp_file "illuaminate" ".json" in
  let headers =
    [ ("Accept", "application/vnd.github.antiope-preview+json");
      ("Authorization", "token " ^ token)
    ]
    |> CCList.flat_map (fun (h, v) -> [ "-H"; Printf.sprintf "%s: %s" h v ])
  in
  let args =
    headers @ [ "-s"; "-X"; mthd; "-d"; body; path ] |> List.map Filename.quote |> String.concat " "
  in
  let cmd = Printf.sprintf "curl %s > %s" args (Filename.quote tmpfile) in

  Log.info (fun f -> f "Running %s" cmd);
  let ok = Sys.command cmd in
  let contents = read_and_delete tmpfile in
  match (ok, contents) with
  | 0, Some body -> (
    try Ok (Yojson.Safe.from_string body)
    with Yojson.Json_error _ ->
      Error (Printf.sprintf "Cannot parse response of %s (%s)" path body) )
  | 0, None -> Error (Printf.sprintf "Cannot read command output")
  | n, _ -> Error (Printf.sprintf "curl exited with %d" n)

let publish_errors ~token ~repo ~sha ~errors =
  let error_list = Error.errors errors in
  let output = Core.Consts.get_skeleton_output error_list in
  let rec update id = function
    | [] -> Ok ()
    | ans :: anss ->
        let body =
          Core.Consts.make_update { output with annotations = ans }
          |> Core.check_run_update_to_yojson
        in
        request ~token
          ~path:(Printf.sprintf "%s/repos/%s/check-runs/%d" root repo id)
          ~mthd:"PATCH" ~body
        >>= fun _ -> update id anss
  in
  let ok = not (Error.has_problems errors) in
  let ans, anss =
    match Core.Consts.to_annotations error_list with
    | [] -> ([], [])
    | x :: xs -> (x, xs)
  in
  let body =
    Core.Consts.make_create ~sha ~ok { output with annotations = ans }
    |> Core.check_run_create_to_yojson
  in
  ( request ~token ~path:(Printf.sprintf "%s/repos/%s/check-runs" root repo) ~mthd:"POST" ~body
  >>= fun x ->
    Core.check_run_response_of_yojson x
    |> Stdlib.Result.map_error (fun _ ->
           Log.err (fun f -> f "Cannot parse response %s" (Yojson.Safe.pretty_to_string x));
           "Cannot parse response") )
  >>= fun { id; _ } -> update id anss
