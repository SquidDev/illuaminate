open Linter
open IlluaminateCore
open IlluaminateSemantics
module E = Doc.Extract

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "doc:detached-comment"

let linter =
  make_no_opt ~tags:[ tag ]
    ~program:(fun () context prog ->
      IlluaminateData.need context.data E.key prog
      |> E.detached_comments
      |> List.map (fun { Doc.Comment.source; _ } ->
             note ~span:source ~tag "Detached doc comment, this will not be processed"))
    ()
