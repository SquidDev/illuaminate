open Linter
open IlluaminateCore
open IlluaminateSemantics

open struct
  module D = Doc.Parser.Data
end

let linter =
  make_no_opt ~tags:Doc.Parser.Tag.all
    ~program:(fun () context prog ->
      Data.get prog D.key context.data |> D.comments
      |> CCList.flat_map (fun (x : Doc.Comment.comment) ->
             List.map (fun (tag, msg) -> note ~span:x.source ~tag "%s" msg) x.Doc.Comment.errors))
    ()
