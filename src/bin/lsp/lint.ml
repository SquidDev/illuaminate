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

let note_to_diagnostic : Driver.Note.any -> Diagnostic.t = function
  | Note { message; detail; tag; span; _ } ->
      let message =
        match detail with
        | None -> message
        | Some m -> Format.asprintf "%s\n%t" message m
      in
      diagnostic ~tag ~span message

module Seq = struct
  include Seq

  let hd (f : 'a Seq.t) : 'a =
    match f () with
    | Cons (x, _) -> x
    | Nil -> failwith "hd"
end

let notes =
  D.Programs.key ~name:__MODULE__ @@ fun data prog ->
  let tags, store = D.need data Store.linters (Node.span prog.eof |> Span.filename) in
  let n, notes =
    List.fold_left
      (fun (n, notes) linter ->
        let this = Driver.need_lint ~tags ~store ~data linter prog in
        let size = Driver.Notes.size this in
        if size > 0 then (n + size, this :: notes) else (n, notes))
      (0, []) Linters.all
  in

  if n = 0 then [||]
  else
    let out = List.hd notes |> Driver.Notes.to_seq |> Seq.hd |> Array.make n in
    List.fold_left
      (fun i xs -> Driver.Notes.to_seq xs |> Seq.fold_left (fun i x -> out.(i) <- x; i + 1) i)
      0 notes
    |> ignore;
    out

let diagnostics store : Store.document -> Diagnostic.t list = function
  | { program = Error { span; value }; _ } ->
      [ diagnostic ~span ~tag:IlluaminateParser.Error.tag
          (Format.asprintf "%a" IlluaminateParser.Error.pp value)
      ]
  | { program = Ok prog; _ } ->
      D.get (Store.data store) notes prog
      |> Array.to_seq |> Seq.map note_to_diagnostic |> CCList.of_std_seq_rev

let to_code_action ~program (i, (Driver.Note.Note { message; fix; _ } as note)) =
  match fix with
  | Nothing -> None
  | One _ | Block _ ->
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
  [@@coverage off]

let code_actions store program range : CodeActionResult.t =
  D.get (Store.data store) notes program
  |> Array.to_seqi
  |> Seq.filter (fun (_, Driver.Note.Note { span; _ }) -> Pos.overlaps range span)
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
    let (Driver.Note.Note { fix; source; kind; _ }) = notes.(id) in
    match fix with
    | Nothing -> Error "No fixer for this note"
    | One f -> Result.map (make_edit source kind) (f source)
    | Block f ->
        f source
        |> Result.map @@ fun res ->
           let range = get_whole_range source kind in
           { range; TextEdit.newText = Format.asprintf "%a" Emit.block res }
