open IlluaminateCore
open Lsp
open Lsp.Types

let span_start span : Position.t =
  { line = Span.start_line span - 1; character = Span.start_col.get span - 1 }

let span_finish span : Position.t =
  { line = Span.finish_line span - 1; character = Span.finish_col.get span }

let range span = Range.create ~start:(span_start span) ~end_:(span_finish span)

let location (span : Span.t) =
  Location.create
    ~uri:(Span.filename span |> Store.Filename.to_uri |> Uri.to_string)
    ~range:(range span)

let severity : Error.level -> DiagnosticSeverity.t = function
  | Critical | Error -> Error
  | Warning -> Warning
  | Note -> Hint

let tag_severity t = severity t.Error.Tag.level

let tag_code t : Jsonrpc.Id.t = Left t.Error.Tag.name

module Pos = struct
  open Position

  let ( <= ) (l : t) (r : t) =
    if l.line == r.line then l.character <= r.character else l.line < r.line

  (** Returns true if [pos] occurs within [span] *)
  let contains pos span = span_start span <= pos && pos <= span_finish span

  (** Returns true if [span] and [range] overlap at all. *)
  let overlaps { Range.start; end_ } span = span_start span <= end_ && start <= span_finish span
end
