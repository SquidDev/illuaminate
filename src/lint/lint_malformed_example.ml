open Linter
open IlluaminateCore
open IlluaminateSemantics
open Lens
open! Doc.Syntax
module E = Doc.Extract

let tag = Error.Tag.make ~attr:[ Default ] ~level:Warning "doc:malformed-example"

let check ~r ~spanner contents =
  let file = Illuaminate.File_id.mk "=input" in
  let program = Lexing.from_string contents |> IlluaminateParser.program file
  and expr = lazy (Lexing.from_string contents |> IlluaminateParser.repl_exprs file) in

  match (program, expr) with
  | Ok _, _ | Error _, (lazy (Ok _)) -> ()
  | Error error1, (lazy (Error error2)) ->
      let error = if Span.compare error1.span error2.span <= 0 then error2 else error1 in
      let detail out = (IlluaminateParser.Error.to_error error).message out () in
      let span = spanner error.span in
      r.r ~span ~detail ~tag "Cannot parse example"

let rec block_line_offset offset : Cmarkit.Block_line.t list -> int option = function
  | [] -> None
  | (x, m) :: xs ->
      let len = String.length x in
      if offset <= len then
        let loc = Cmarkit.Meta.textloc m in
        if Cmarkit.Textloc.is_none loc then None else Some (Cmarkit.Textloc.first_byte loc + offset)
      else block_line_offset (offset - len - 1) xs

let check_code_blocks ~r ~lines md =
  let module C = Cmarkit.Block.Code_block in
  let language b =
    Option.bind (C.info_string b) (fun (x, _) -> C.language_of_info_string x) |> Option.map fst
  in
  let block _ () node =
    match node with
    | Cmarkit.Block.Code_block (b, _) ->
        (match language b with
        | Some "lua" ->
            let code = C.code b in
            let spanner x =
              let module Cl = Doc.AbstractSyntax.Comment_lines in
              match
                ( block_line_offset (x ^. Span.start_offset) code,
                  block_line_offset (x ^. Span.finish_offset) code )
              with
              | Some start, Some finish -> Cl.span_of_range lines start finish
              | _ -> Cl.span lines
            in
            Cmarkit_ext.block_lines_contents code |> check ~r ~spanner
        | _ -> ());
        Cmarkit.Folder.ret ()
    | _ -> Cmarkit.Folder.default
  in
  let folder = Cmarkit.Folder.make ~block () in
  Doc.Syntax.Markdown.doc md |> Cmarkit.Folder.fold_doc folder ()

let check_abstract ~r =
  object
    inherit abstract_iter as super

    method! description { description; description_pos } =
      check_code_blocks ~r ~lines:description_pos description

    method! example =
      function
      | RawExample { span; value } ->
          let spanner x =
            span
            |> Span.start_offset %= ( + ) (x ^. Span.start_offset)
            |> Span.finish_offset %= ( + ) (x ^. Span.finish_offset)
          in
          check ~r ~spanner value
      | RichExample _ as e -> super#example e
  end

let linter =
  make_no_opt ~tags:[ tag ]
    ~file:(fun () context r _ ->
      match IlluaminateData.need context.data E.file context.file |> Option.get |> E.get_page with
      | None -> ()
      | Some m ->
          let iter = iter_of (fun ~span:_ -> check_abstract ~r) in
          iter#documented iter#page m)
    ()
