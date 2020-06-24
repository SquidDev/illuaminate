open Linter
open IlluaminateCore
open IlluaminateCore.Syntax
open IlluaminateSemantics
module D = Doc.Parser.Data
module Config = Doc.Extract.Config

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
           config.module_kinds)
      in

      IlluaminateData.need context.data D.key prog
      |> D.comments
      |> List.iter @@ fun (x : Doc.Comment.comment) ->
         x.Doc.Comment.errors |> List.iter (fun (tag, span, msg) -> r.r ~span ~tag "%s" msg);
         match x.module_info with
         | Some { value = { mod_kind = Some (Custom kind); _ }; span }
           when not (List.exists (fun { Config.id; _ } -> id = kind) (Lazy.force module_kinds)) ->
             r.r ~span ~tag:tag_module_kind "Unknown module kind %S" kind;
             ()
         | _ -> ())
    ()
