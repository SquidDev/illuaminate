open Linter
open IlluaminateSemantics
module D = Doc.Parser.Data

let linter =
  make_no_opt ~tags:Doc.Parser.Tag.all
    ~program:(fun () context r prog ->
      IlluaminateData.need context.data D.key prog
      |> D.comments
      |> List.iter @@ fun (x : Doc.Comment.comment) ->
         x.Doc.Comment.errors |> List.iter @@ fun (tag, span, msg) -> r.r ~span ~tag "%s" msg)
    ()
