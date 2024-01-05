module E = IlluaminateSemantics.Doc.Extract

let linter =
  Linter.make_no_opt ~tags:[]
    ~file:(fun () context r _ ->
      IlluaminateData.need context.data E.file context.file
      |> Option.get |> E.errors
      |> List.iter (fun error -> r.x (E.Extract_error.to_error error)))
    ()
