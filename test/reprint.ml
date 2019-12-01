open Omnomnom.Tests
open IlluaminateCore

let process name =
  let path = Filename.concat "data/reprint" name in
  let contents = CCIO.(with_in path read_all) in
  match Lexing.from_string contents |> IlluaminateParser.parse { Span.name; path }with
  | Error err ->
      let errs = Error.make () in
      IlluaminateParser.Error.report errs err.span err.value;
      Error.display_of_string (fun _ -> Some contents) errs;
      result (Failed { backtrace = None })
  | Ok program ->
      let new_contents = Format.asprintf "%a" Emit.program program in
      if contents <> new_contents then
        result
          ~message:(Some (fun out -> Helpers.diff out contents new_contents))
          (Failed { backtrace = None })

      else result Pass


let tests =
  try
    let names = ref [] in
    let handle = Unix.opendir "data/reprint" in
    ( try
        while true do
          let child = Unix.readdir handle in
          if child <> "." && child <> ".." && Filename.extension child = ".lua" then
            names := child :: !names
        done
      with End_of_file -> () );
    !names |> List.sort String.compare
    |> List.map (fun child -> test child (fun () -> process child))
    |> group "Reprint"
  with e ->
    let res = result_of_exn e in
    test "Reprint" (fun () -> res)
