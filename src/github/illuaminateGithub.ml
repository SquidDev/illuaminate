let getenv name err =
  match Sys.getenv_opt name with
  | Some x -> Ok x
  | None -> Error err

let publish_errors errors =
  let ( let+ ) = Result.bind in
  let+ token = getenv "GITHUB_TOKEN" "GITHUB_TOKEN is not available." in
  let+ sha = getenv "GITHUB_SHA" "GITHUB_SHA is not available." in
  let+ repo = getenv "GITHUB_REPOSITORY" "GITHUB_REPOSITORY is not available." in
  Network.publish_errors ~token ~sha ~repo ~errors
