let load_file ~options path =
  let open Illuaminate.Html in
  match CCIO.File.read (Fpath.to_string path) with
  | Error msg ->
      Format.asprintf "Cannot open documentation index '%a' (%s)\n%!" Fpath.pp path msg
      |> Result.error
  | Ok contents -> (
    match Fpath.get_ext path with
    | ".html" | ".htm" -> raw contents |> Result.ok
    | ".md" | ".markdown" ->
        let module R = IlluaminateSemantics.Reference in
        let module D = IlluaminateSemantics.Doc in
        let module Lift = D.AbstractSyntax.Lift (D.Comment) (D.Syntax) in
        let lifter =
          { Lift.any_ref = (fun (Reference r) -> (None, R.Unknown r));
            type_ref = (fun (Reference r) -> R.Unknown r)
          }
        in
        IlluaminateSemantics.Doc.Parser.parse_description contents
        |> Lift.markdown lifter |> Html_md.md ~path ~options |> Result.ok
    | ".txt" | "" -> create_node ~tag:"pre" ~children:[ str contents ] () |> Result.ok
    | ext ->
        Format.asprintf "Cannot handle documentation index '%a' (unknown file extension %S)\n%!"
          Fpath.pp path ext
        |> Result.error)
