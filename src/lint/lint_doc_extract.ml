open Linter
open IlluaminateCore
open IlluaminateSemantics
module E = Doc.Extract

let linter =
  make_no_opt ~tags:E.Tag.all
    ~program:(fun () context prog ->
      IlluaminateData.need context.data E.key prog
      |> E.errors
      |> List.map (fun { Error.Error.span; tag; message; _ } -> note ~span ~tag "%s" message))
    ()
