(** Make a request against the GitHub API. *)
val publish_errors :
  token:string ->
  repo:string ->
  sha:string ->
  errors:IlluaminateCore.Error.t ->
  (unit, string) result
