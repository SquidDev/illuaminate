open Linter
open IlluaminateCore
open IlluaminateSemantics
module E = Doc.Extract

let linter =
  make_no_opt ~tags:E.Tag.all
    ~program:(fun () context r prog ->
      IlluaminateData.need context.data E.key prog
      |> E.errors
      |> List.iter (fun { Error.Error.span; tag; message; _ } -> r.r ~span ~tag "%s" message))
    ()
