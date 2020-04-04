open Stdlib
open IlluaminateCore
open IlluaminateLint
open Lsp.Types
open Lsp_convert
open Syntax
module D = IlluaminateData

let fname p = Syntax.Spanned.program p |> Span.filename

let diagnostic ~tag ~span ?relatedInformation message =
  Diagnostic.create ~range:(range span) ~severity:(tag_severity tag) ~code:(tag_code tag)
    ~source:"illuaminate" ~message ?relatedInformation ?tags:(tag_attributes tag) ()

let note_to_diagnostic : Driver.any_note -> Diagnostic.t = function
  | Note ({ note = { Linter.message; detail; tag; _ }; _ } as n) ->
      let span = Driver.NoteAt.span n in
      let message =
        match detail with
        | None -> message
        | Some m -> Format.asprintf "%s\n%t" message m
      in
      diagnostic ~tag ~span message

let notes =
  D.Programs.key ~name:__MODULE__ @@ fun data prog ->
  let tags, store = D.need data Store.linters (Node.span prog.eof |> Span.filename) in
  let notes =
    List.fold_left
      (fun notes linter ->
        match Driver.need_lint ~tags ~store ~data linter prog with
        | [] -> notes
        | n -> n :: notes)
      [] Linters.all
  in
  match notes with
  | [] -> [||]
  | (x :: xs) :: xss ->
      let length = List.fold_left (fun n x -> n + List.length x) 0 notes in
      let out = Array.make length x in
      let rec add i xs xss =
        match (xs, xss) with
        | [], [] -> ()
        | [], xs :: xss -> add i xs xss
        | x :: xs, xss ->
            out.(i) <- x;
            add (i + 1) xs xss
      in
      add 1 xs xss; out
  | [] :: _ -> failwith "Impossible"

let diagnostics store : Store.document -> Diagnostic.t list = function
  | { program = Error { span; value }; _ } ->
      [ diagnostic ~span ~tag:IlluaminateParser.Error.tag
          (Format.asprintf "%a" IlluaminateParser.Error.pp value)
      ]
  | { program = Ok prog; _ } ->
      D.get (Store.data store) notes prog
      |> Array.to_seq |> Seq.map note_to_diagnostic |> CCList.of_std_seq_rev

let to_code_action ~program (i, (Driver.Note { note = { Linter.message; fix; _ }; _ } as note)) =
  match fix with
  | FixNothing -> None
  | FixOne _ | FixBlock _ ->
      let title = Printf.sprintf "Fix '%s'" message in
      let action =
        CodeAction.create ~title ~kind:QuickFix ~diagnostics:[ note_to_diagnostic note ]
          ~isPreferred:true
          ~command:
            (Command.create ~title ~command:"illuaminate/fix"
               ~arguments:[ Store.Filename.to_uri_json (fname program); `Int i ]
               ())
          ()
      in
      Some (`CodeAction action)
  (* bisect_ppx doesn't work well with GADTs *) [@@coverage off]

let code_actions store program range : CodeActionResult.t =
  D.get (Store.data store) notes program
  |> Array.to_seqi
  |> Seq.filter (fun (_, Driver.Note n) -> Pos.overlaps range (Driver.NoteAt.span n))
  |> Seq.filter_map (to_code_action ~program)
  |> CCList.of_std_seq_rev |> Option.some

let get_whole_range before witness =
  let start = (Witness.first witness).get before and finish = (Witness.first witness).get before in
  let start =
    match start with
    | SimpleNode _ -> assert false
    | Node { span; leading_trivia = []; _ } | Node { leading_trivia = { span; _ } :: _; _ } -> span
  and finish =
    match finish with
    | SimpleNode _ -> assert false
    | Node { span; trailing_trivia; _ } -> (
      match CCList.last_opt trailing_trivia with
      | None -> span
      | Some { span; _ } -> span )
  in
  { Range.start = span_start start; end_ = span_finish finish }

let make_edit (type a) (before : a) (witness : a Witness.t) (after : a) : TextEdit.t =
  let range = get_whole_range before witness in
  let newText = Format.asprintf "%a" (Witness.emit witness) after in
  { range; newText }

let fix store program id =
  let notes = D.get (Store.data store) notes program in
  if id < 0 || id > Array.length notes then Result.Error "Unknown note"
  else
    let (Driver.Note { note = { fix; _ }; source; witness }) = notes.(id) in
    match fix with
    | FixNothing -> Error "No fixer for this note"
    | FixOne f -> Result.map (make_edit source witness) (f source)
    | FixBlock f ->
        f source
        |> Result.map @@ fun res ->
           let range = get_whole_range source witness in
           { range; TextEdit.newText = Format.asprintf "%a" Emit.block res }
