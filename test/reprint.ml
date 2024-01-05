open Omnomnom.Tests
open Illuaminate
open IlluaminateCore

let process name =
  let path = Filename.concat "data/reprint" name in
  let contents = CCIO.(with_in path read_all) in
  let name = File_id.mk name in
  match Lexing.from_string contents |> IlluaminateParser.program name with
  | Error err ->
      Illuaminate.Console_reporter.display_of_string ~with_summary:false
        (fun _ -> Some contents)
        [ IlluaminateParser.Error.to_error err ];
      result (Failed { backtrace = None })
  | Ok program ->
      let new_contents = Format.asprintf "%a" Emit.program program in
      if contents <> new_contents then
        result
          ~message:(fun out -> Helpers.diff out contents new_contents)
          (Failed { backtrace = None })
      else result Pass

let tests =
  try
    Sys.readdir "data/reprint" |> Array.to_list |> List.sort String.compare
    |> List.map (fun child -> test child (fun () -> process child))
    |> group "Reprint"
  with e ->
    let res = result_of_exn e in
    test "Reprint" (fun () -> res)
