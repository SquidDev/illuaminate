open IlluaminateCore
open IlluaminateMinify
module D = IlluaminateData

let process ~name contents =
  let lexbuf = Lexing.from_string contents in
  let name = Span.Filename.mk name in
  let errs = Error.make () in
  let contents' =
    match IlluaminateParser.program name lexbuf with
    | Error err ->
        IlluaminateParser.Error.report errs err.span err.value;
        ""
    | Ok parsed ->
        let msg =
          Format.asprintf "%t" @@ fun out ->
          D.compute (fun ctx -> IlluaminateMinify.minify ctx parsed) D.Builder.(build empty)
          |> Emit.use out Emit.program
        in
        let lexbuf = Lexing.from_string contents in
        ( match IlluaminateParser.program name lexbuf with
        | Ok _ -> ()
        | Error err -> IlluaminateParser.Error.report errs err.span err.value );
        msg
  in

  Format.asprintf "%t" @@ fun out ->
  Format.fprintf out "%s\n" contents';
  if Error.has_problems errs then (
    Format.pp_print_string out "--[==[\n";
    Error.display_of_string ~out ~with_summary:false (fun _ -> Some contents) errs;
    Format.pp_print_string out "]==]\n" )

let tests =
  OmnomnomGolden.of_directory process ~group:"Minify" ~directory:"data/minify" ~extension:".lua" ()
