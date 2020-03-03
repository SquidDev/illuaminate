open Linter
open IlluaminateCore
open IlluaminateSemantics
module E = Doc.Extract

let linter =
  let tag = Error.Tag.make Error.Warning "doc:detached-comment" in
  make_no_opt ~tags:[ tag ]
    ~program:(fun () context prog ->
      IlluaminateData.get context.data E.key prog
      |> E.detached_comments
      |> List.map (fun { Doc.Comment.source; _ } ->
             note ~span:source ~tag "Detached doc comment, this will not be processed"))
    ()
