let exec cmd _ = Printf.sprintf "Cannot run external commands such as %s" cmd |> Result.error
