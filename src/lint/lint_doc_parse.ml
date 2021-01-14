open Linter
open IlluaminateCore
open IlluaminateCore.Syntax
open IlluaminateSemantics
module D = Doc.Parser.Data
module Config = Doc.Extract.Config
module NSet = Set.Make (Namespace)

(** Strictly speaking this is actually namespaces. The public-facing nomenclature is a little
    different.*)
let tag_module_kind = Error.Tag.make ~attr:[ Default ] ~level:Error "doc:unknown-module-kind"

let linter =
  make_no_opt
    ~tags:(tag_module_kind :: Doc.Parser.Tag.all)
    ~program:(fun () context r prog ->
      let module_kinds =
        lazy
          (let context =
             IlluaminateData.(
               need context.data Programs.Context.key
                 (Spanned.program context.program |> Span.filename))
           in
           let config = IlluaminateConfig.Schema.get Config.key context.config in
           Namespace.module_ :: Namespace.library
           :: List.map (fun x -> Namespace.Namespace x.Config.id) config.module_kinds
           |> NSet.of_list)
      in

      IlluaminateData.need context.data D.key prog
      |> D.comments
      |> List.iter @@ fun (x : Doc.Comment.comment) ->
         x.Doc.Comment.errors |> List.iter (fun (tag, span, msg) -> r.r ~span ~tag "%s" msg);
         match x.module_info with
         | Some { value = { mod_namespace = Some namespace; _ }; span }
           when not (NSet.mem namespace (Lazy.force module_kinds)) ->
             let (Namespace k) = namespace in
             r.r ~span ~tag:tag_module_kind "Unknown module kind %S" k;
             ()
         | _ -> ())
    ()
