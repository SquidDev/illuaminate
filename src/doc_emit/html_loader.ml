let load_file ~resolve path =
  let open Html.Default in
  match CCIO.File.read (Fpath.to_string path) with
  | Error msg ->
      Format.asprintf "Cannot open documentation index '%a' (%s)\n%!" Fpath.pp path msg
      |> Result.error
  | Ok contents -> (
    match Fpath.get_ext path with
    | ".html" | ".htm" -> raw contents |> Result.ok
    | ".md" | ".markdown" ->
        let x = IlluaminateSemantics.Doc.Parser.parse_description contents in
        Html_md.md ~resolve x |> Result.ok
    | ".txt" | "" -> create_node ~tag:"pre" ~children:[ str contents ] () |> Result.ok
    | ext ->
        Format.asprintf "Cannot handle documentation index '%a' (unknown file extension %S)\n%!"
          Fpath.pp path ext
        |> Result.error )
