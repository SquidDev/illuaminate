open Linter
open IlluaminateCore
open IlluaminateSemantics
module E = Doc.Extract

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "doc:detached-comment"

let linter =
  make_no_opt ~tags:[ tag ]
    ~file:(fun () context r _ ->
      IlluaminateData.need context.data E.file context.file
      |> Option.get |> E.detached_comments
      |> List.iter (fun { Doc.Comment.source; _ } ->
             r.r ~span:source ~tag "Detached doc comment, this will not be processed"))
    ()
