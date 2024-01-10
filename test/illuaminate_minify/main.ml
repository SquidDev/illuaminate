let () =
  Md_test_runner.run @@ fun out _ contents ->
  let name = Illuaminate.File_id.mk "=input" in

  match IlluaminateParser.program name (Lexing.from_string contents) with
  | Error err ->
      Illuaminate.Console_reporter.display_of_string ~out ~with_summary:false
        (fun _ -> Some contents)
        [ IlluaminateParser.Error.to_error err ]
  | Ok parsed -> (
      (* Minify our program. *)
      let updated =
        let buffer = Buffer.create 512 in
        let out = Format.formatter_of_buffer buffer in
        (IlluaminateMinify.minify parsed
        |> IlluaminateMinify.Emit.(with_wrapping out "%a@\n" program));
        Buffer.contents buffer
      in

      Format.pp_print_string out updated;

      (* Attempt to reparse it. This doesn't check the two are syntactically equivalent, only that
         it is syntactically valid. *)
      match IlluaminateParser.program name (Lexing.from_string updated) with
      | Ok _ -> ()
      | Error err ->
          Format.pp_print_string out "\n--[==[\n";
          Illuaminate.Console_reporter.display_of_string ~out ~with_summary:false
            (fun _ -> Some updated)
            [ IlluaminateParser.Error.to_error err ];
          Format.pp_print_string out "]==]\n")
