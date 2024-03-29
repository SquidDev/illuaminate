open Linter
open IlluaminateCore
open IlluaminateSemantics
module D = Doc.Parser.Data
module Config = Doc.Extract.Config
module NSet = Set.Make (Namespace)

(** Strictly speaking this is actually namespaces. The public-facing nomenclature is a little
    different.*)
let tag_module_kind = Error.Tag.make ~attr:[ Default ] ~level:Error "doc:unknown-module-kind"

let linter =
  make_no_opt ~tags:[]
    ~file:(fun () context r _ ->
      let module_kinds =
        lazy
          (let context = IlluaminateData.(need context.data Programs.Context.key context.file) in
           let config = IlluaminateConfig.Schema.get Config.key context.config in
           Namespace.module_ :: Namespace.library
           :: List.map (fun x -> Namespace.Namespace x.Config.id) config.module_kinds
           |> NSet.of_list)
      in

      IlluaminateData.need context.data D.file context.file
      |> Option.get |> D.comments
      |> List.iter @@ fun (x : Doc.Comment.comment) ->
         List.iter
           (fun error -> r.x (Doc.Comment.Comment_error.to_error error))
           x.Doc.Comment.errors;
         match x.module_info with
         | Some { value = { mod_namespace = Some namespace; _ }; span }
           when not (NSet.mem namespace (Lazy.force module_kinds)) ->
             let (Namespace k) = namespace in
             r.r ~span ~tag:tag_module_kind "Unknown module kind %S" k;
             ()
         | _ -> ())
    ()
