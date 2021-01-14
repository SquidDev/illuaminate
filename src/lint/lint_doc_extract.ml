open Linter
open IlluaminateCore
open IlluaminateSemantics
module E = Doc.Extract

let linter =
  make_no_opt ~tags:E.Tag.all
    ~file:(fun () context r prog ->
      IlluaminateData.need context E.file prog
      |> E.errors
      |> List.iter (fun { Error.Error.span; tag; message; _ } -> r.r ~span ~tag "%s" message))
    ()
