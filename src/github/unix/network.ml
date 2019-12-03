open IlluaminateCore
open Cohttp
open Cohttp_lwt_unix

let root = "https://api.github.com"

let ( let> ) = Lwt.( >>= )

let ( let+ ) = Lwt.( >|= )

(** Make a request against the GitHub API. *)
let request ~token ~path ~mthd ~body : (Yojson.Safe.t, string) result Lwt.t =
  let uri = Uri.of_string path in
  let body = `String (Yojson.Safe.to_string ~std:true body) in
  let headers =
    Header.add_list (Header.init ())
      [ ("Accept", "application/vnd.github.antiope-preview+json");
        ("Authorization", "token " ^ token)
      ]
  in
  let> response, body = Client.call ~headers ~body mthd uri in
  let code = Response.status response |> Code.code_of_status in
  let+ body = Cohttp_lwt.Body.to_string body in
  if not (Code.is_success code) then
    Error (Printf.sprintf "Error making request to %s (%s)" path body)
  else
    try Ok (Yojson.Safe.from_string body)
    with Yojson.Json_error _ ->
      Error (Printf.sprintf "Cannot parse response of %s (%s)" path body)

let publish_errors ~token ~repo ~sha ~errors =
  let error_list = Error.errors errors in
  let output = Core.Consts.get_skeleton_output error_list in
  let rec update id = function
    | [] -> Lwt.return (Ok ())
    | ans :: anss -> (
        let body =
          Core.Consts.make_update { output with annotations = ans }
          |> Core.check_run_update_to_yojson
        in
        let> result =
          request ~token
            ~path:(Printf.sprintf "%s/repos/%s/check-runs/%d" root repo id)
            ~mthd:`PATCH ~body
        in
        match result with
        | Error e -> Lwt.return (Error e)
        | Ok _ -> update id anss )
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
  Lwt_main.run
  @@ let> result =
       request ~token ~path:(Printf.sprintf "%s/repos/%s/check-runs" root repo) ~mthd:`POST ~body
     in
     match Stdlib.Result.map Core.check_run_response_of_yojson result with
     | Error e -> Lwt.return (Error e)
     | Ok (Error e) -> Lwt.return (Error e)
     | Ok (Ok { id; _ }) -> update id anss
