let load_file ~options path =
  let open Html.Default in
  match CCIO.File.read (Fpath.to_string path) with
  | Error msg ->
      Format.asprintf "Cannot open documentation index '%a' (%s)\n%!" Fpath.pp path msg
      |> Result.error
  | Ok contents -> (
    match Fpath.get_ext path with
    | ".html" | ".htm" -> raw contents |> Result.ok
    | ".md" | ".markdown" ->
        let module R = IlluaminateSemantics.Reference in
        IlluaminateSemantics.Doc.Parser.parse_description contents
        |> IlluaminateSemantics__Omd_transform.Map.doc (fun (R.Reference r) -> (None, R.Unknown r))
        |> Html_md.md ~options |> Result.ok
    | ".txt" | "" -> create_node ~tag:"pre" ~children:[ str contents ] () |> Result.ok
    | ext ->
        Format.asprintf "Cannot handle documentation index '%a' (unknown file extension %S)\n%!"
          Fpath.pp path ext
        |> Result.error)
