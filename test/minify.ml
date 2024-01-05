open IlluaminateMinify

let process ~name contents =
  let lexbuf = Lexing.from_string contents in
  let name = Illuaminate.File_id.mk name in
  Format.asprintf "%t" @@ fun out ->
  match IlluaminateParser.program name lexbuf with
  | Error err ->
      Illuaminate.Console_reporter.display_of_string ~out ~with_summary:false
        (fun _ -> Some contents)
        [ IlluaminateParser.Error.to_error err ]
  | Ok parsed -> (
      let updated =
        let buffer = Buffer.create 512 in
        let out = Format.formatter_of_buffer buffer in
        (minify parsed |> Emit.(with_wrapping out "%a" program));
        Buffer.contents buffer
      in
      Format.pp_print_string out updated;

      let lexbuf = Lexing.from_string updated in
      match IlluaminateParser.program name lexbuf with
      | Ok _ -> ()
      | Error err ->
          Format.pp_print_string out "\n--[==[\n";
          Illuaminate.Console_reporter.display_of_string ~out ~with_summary:false
            (fun _ -> Some updated)
            [ IlluaminateParser.Error.to_error err ];
          Format.pp_print_string out "]==]\n")

let tests =
  OmnomnomGolden.of_directory process ~group:"Minify" ~directory:"data/minify" ~extension:".lua" ()
