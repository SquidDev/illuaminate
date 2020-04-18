open Github_types
open IlluaminateCore
open CCResult.Infix

let src = Logs.Src.create ~doc:"Makes requests to the GH actions API" __MODULE__

module Log = (val Logs.src_log src)

let root = "https://api.github.com"

(** Make a request against the GitHub API. *)
let request ~token ~path ~mthd ~body : (Yojson.Safe.t, string) result =
  let body = Yojson.Safe.to_string ~std:true body in
  let headers =
    [ ("Accept", "application/vnd.github.antiope-preview+json");
      ("Authorization", "token " ^ token)
    ]
    |> CCList.flat_map (fun (h, v) -> [ "-H"; Printf.sprintf "%s: %s" h v ])
  in
  let args = headers @ [ "-s"; "-X"; mthd; "-d"; body; path ] in
  Log.info (fun f -> f "Running curl %s" (List.map Filename.quote args |> String.concat " "));
  match IlluaminateExec.exec "curl" ("curl" :: args |> Array.of_list) with
  | Ok body -> (
    try Ok (Yojson.Safe.from_string body)
    with Yojson.Json_error _ ->
      Error (Printf.sprintf "Cannot parse response of %s (%s)" path body) )
  | Error e -> Error e

let publish_errors ~token ~repo ~sha ~errors =
  let error_list = Error.errors errors in
  let output = Consts.get_skeleton_output error_list in
  let rec update id = function
    | [] -> Ok ()
    | ans :: anss ->
        let body =
          Consts.make_update { output with annotations = ans } |> check_run_update_to_yojson
        in
        request ~token
          ~path:(Printf.sprintf "%s/repos/%s/check-runs/%d" root repo id)
          ~mthd:"PATCH" ~body
        >>= fun _ -> update id anss
  in
  let ok = not (Error.has_problems errors) in
  let ans, anss =
    match Consts.to_annotations error_list with
    | [] -> ([], [])
    | x :: xs -> (x, xs)
  in
  let body =
    Consts.make_create ~sha ~ok { output with annotations = ans } |> check_run_create_to_yojson
  in
  ( request ~token ~path:(Printf.sprintf "%s/repos/%s/check-runs" root repo) ~mthd:"POST" ~body
  >>= fun x ->
    check_run_response_of_yojson x
    |> Result.map_error (fun _ ->
           Log.err (fun f -> f "Cannot parse response %s" (Yojson.Safe.pretty_to_string x));
           "Cannot parse response") )
  >>= fun { id; _ } -> update id anss

let getenv name err =
  match Sys.getenv_opt name with
  | Some x -> Ok x
  | None -> Error err

let publish_errors errors =
  let ( let+ ) = Result.bind in
  let+ token = getenv "GITHUB_TOKEN" "GITHUB_TOKEN is not available." in
  let+ sha = getenv "GITHUB_SHA" "GITHUB_SHA is not available." in
  let+ repo = getenv "GITHUB_REPOSITORY" "GITHUB_REPOSITORY is not available." in
  publish_errors ~token ~sha ~repo ~errors
