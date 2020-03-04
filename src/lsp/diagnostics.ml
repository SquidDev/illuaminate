open IlluaminateLint
open Lsp.Protocol
open! PublishDiagnostics
open Lsp_convert

let diagnostic ~tag ~span ?(relatedInformation = []) ?(tags = []) message =
  { range = range span;
    severity = tag_severity tag;
    code = tag_code tag;
    source = Some "illuaminate";
    message;
    relatedInformation;
    tags
  }

let diagnostic_to_note : Driver.any_note -> diagnostic = function
  | Note n ->
      let { Linter.message; detail; tag; _ } = Driver.NoteAt.note n in
      let span = Driver.NoteAt.span n in
      let message =
        match detail with
        | None -> message
        | Some m -> Format.asprintf "%s\n%t" message m
      in
      diagnostic ~tag ~span message

let lint store : Store.document -> diagnostic list = function
  | { program = Error { span; value }; _ } ->
      [ diagnostic ~span ~tag:IlluaminateParser.Error.tag
          (Format.asprintf "%a" IlluaminateParser.Error.pp value)
      ]
  | { context = { config; _ }; program = Ok prog; _ } ->
      let data = Store.data store in
      Linters.all |> List.to_seq
      |> Seq.flat_map (fun linter -> Driver.lint ~store:config ~data linter prog |> List.to_seq)
      |> Seq.map diagnostic_to_note |> CCList.of_std_seq_rev
