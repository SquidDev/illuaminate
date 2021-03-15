open IlluaminateCore
open IlluaminateMinify
module D = IlluaminateData

let process ~name contents =
  let lexbuf = Lexing.from_string contents in
  let name = Span.Filename.mk name in
  let errs = Error.make () in
  Format.asprintf "%t" @@ fun out ->
  match IlluaminateParser.program name lexbuf with
  | Error err ->
      IlluaminateParser.Error.report errs err.span err.value;
      Error.display_of_string ~out ~with_summary:false (fun _ -> Some contents) errs
  | Ok parsed -> (
      let updated =
        let buffer = Buffer.create 512 in
        let out = Format.formatter_of_buffer buffer in
        (D.compute (fun ctx -> minify ctx parsed) D.Builder.(build empty)
        |> Emit.(with_wrapping out "%a" program));
        Buffer.contents buffer
      in
      Format.pp_print_string out updated;

      let lexbuf = Lexing.from_string updated in
      match IlluaminateParser.program name lexbuf with
      | Ok _ -> ()
      | Error err ->
          IlluaminateParser.Error.report errs err.span err.value;
          Format.pp_print_string out "\n--[==[\n";
          Error.display_of_string ~out ~with_summary:false (fun _ -> Some updated) errs;
          Format.pp_print_string out "]==]\n")

let tests =
  OmnomnomGolden.of_directory process ~group:"Minify" ~directory:"data/minify" ~extension:".lua" ()
