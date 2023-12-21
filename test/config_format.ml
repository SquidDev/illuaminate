open Alcotest
open IlluaminateCore
module C = IlluaminateConfigFormat

let root = Sys.getcwd () |> Fpath.v

let parse str =
  Lexing.from_string str
  |> C.of_lexer ~directory:root (Illuaminate.File_id.mk "=in")
  |> Result.fold ~ok:Fun.id ~error:(fun { Span.value; span } ->
         Format.asprintf "%a: %s" Span.pp span value |> failwith)

let get_source_link format span =
  let c = Printf.sprintf "(doc (site (source-link %S)))" format |> parse |> C.get_doc_options in
  c.site_properties.source_link (Span span)

let mk_span file =
  let open Span in
  let buf = Lexing.from_string "a\nb" in
  Lines.using file buf @@ fun l ->
  buf.lex_curr_p <- { buf.lex_curr_p with pos_cnum = 2; pos_lnum = 1 };
  Lines.new_line l;
  buf.lex_curr_p <- { buf.lex_curr_p with pos_cnum = 3; pos_lnum = 2 };
  Lines.new_line l;
  let s = { Lexing.pos_bol = 0; pos_lnum = 1; pos_cnum = 0; pos_fname = "" } in
  let f = { Lexing.pos_bol = 0; pos_lnum = 2; pos_cnum = 3; pos_fname = "" } in
  of_pos2 l s f

let span = Illuaminate.File_id.mk ~path:Fpath.(root / "test.lua") "=in" |> mk_span

let () =
  Alcotest.run "Config format"
    [ ( "Documentation source links",
        [ test_case "Supports a single line" `Quick (fun () ->
              Alcotest.(check (option string))
                "Format string is correct"
                (get_source_link "[ ${path} ${line} ]" span)
                (Some "[ test.lua 1 ]"));
          test_case "Supports a line range" `Quick (fun () ->
              Alcotest.(check (option string))
                "Format string is correct"
                (get_source_link "[ ${path} ${sline} ${eline} ]" span)
                (Some "[ test.lua 1 2 ]"));
          test_case "Supports git commits" `Quick (fun () ->
              let sha = get_source_link "${commit}" span |> Option.get in
              Alcotest.(check int)
                (Printf.sprintf "SHA is correct length (%s)" sha)
                (String.length sha) 40;
              let is_hex x = (x >= '0' && x <= '9') || (x >= 'a' && x <= 'f') in
              Alcotest.(check (list char))
                (Printf.sprintf "All characters are hex (%s)" sha)
                (String.to_seq sha |> Seq.filter (fun x -> not (is_hex x)) |> List.of_seq)
                []);
          test_case "Path fails for temporary programs" `Quick (fun () ->
              Alcotest.(check (option string))
                "Format string is None"
                (Illuaminate.File_id.mk "=in" |> mk_span |> get_source_link "[ ${path} ]")
                None);
          test_case "Line works for temporary programs" `Quick (fun () ->
              Alcotest.(check (option string))
                "Format string is correct"
                (Illuaminate.File_id.mk "=in" |> mk_span |> get_source_link "[ ${line} ]")
                (Some "[ 1 ]"))
        ] );
      ( "Reprinting",
        [ test_case "Round trips correctly" `Quick (fun () ->
              (* There's not much we can do right not to correctly assert things. *)
              let _roundtrip = Format.asprintf "%t" C.generate |> parse in
              ())
        ] )
    ]
