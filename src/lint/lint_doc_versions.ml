open Linter
open IlluaminateCore
open IlluaminateSemantics
module D = Doc.Parser.Data
module Config = Doc.Extract.Config
module NSet = Set.Make (Namespace)

let tag_unordered_versions = Error.Tag.make ~attr:[ Default ] ~level:Error "doc:unordered-versions"

let check_versions r ({ changes; _ } : Doc.Comment.comment) =
  match changes with
  | [] | [ _ ] -> ()
  | _ :: changes -> (
    match List.find_opt (fun { Doc.Comment.change_kind; _ } -> change_kind = Added) changes with
    | None -> ()
    | Some { change_span; change_version; _ } ->
        (* TODO: We should have a fixer for this. *)
        r.r ~span:change_span ~tag:tag_unordered_versions
          "@since tag for version %S must be the first changelog entry." change_version)

let linter =
  make_no_opt ~tags:[ tag_unordered_versions ]
    ~file:(fun () context r _ ->
      IlluaminateData.need context.data D.file context.file
      |> Option.get |> D.comments
      |> List.iter (check_versions r))
    ()
