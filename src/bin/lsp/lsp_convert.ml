open IlluaminateCore
open Lsp.Types

let span_start { Span.start_line; start_col; _ } : Position.t =
  { line = start_line - 1; character = start_col - 1 }

let span_finish { Span.finish_line; finish_col; _ } : Position.t =
  { line = finish_line - 1; character = finish_col }

let range span = Range.create ~start:(span_start span) ~end_:(span_finish span)

let location ~uri span = Location.create ~uri ~range:(range span)

let severity : Error.level -> DiagnosticSeverity.t = function
  | Critical | Error -> Error
  | Warning -> Warning
  | Note -> Hint

let tag_severity t = severity t.Error.Tag.level

let tag_code t : Lsp.Jsonrpc.Id.t = Left t.Error.Tag.name

module Pos = struct
  open Position

  let ( <= ) (l : t) (r : t) =
    if l.line == r.line then l.character <= r.character else l.line < r.line

  (** Returns true if [pos] occurs within [span] *)
  let contains pos span = span_start span <= pos && pos <= span_finish span

  (** Returns true if [span] and [range] overlap at all. *)
  let overlaps { Range.start; end_ } span = span_start span <= end_ && start <= span_finish span
end
