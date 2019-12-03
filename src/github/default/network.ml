let publish_errors :
    token:string ->
    repo:string ->
    sha:string ->
    errors:IlluaminateCore.Error.t ->
    (unit, string) result =
 fun ~token:_ ~repo:_ ~sha:_ ~errors:_ -> Error "GitHub integration is not supported."
