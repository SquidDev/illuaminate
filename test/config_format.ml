open Omnomnom.Tests
open OmnomnomAlcotest
open IlluaminateCore
module C = IlluaminateConfigFormat

let root = Sys.getcwd () |> Fpath.v

let parse str =
  Lexing.from_string str
  |> C.of_lexer ~directory:root (Span.Filename.mk "=in")
  |> Result.fold ~ok:Fun.id ~error:(fun { Span.value; span } ->
         Format.asprintf "%a: %s" Span.pp span value |> failwith)

let maybe_source_link format span =
  let c = Printf.sprintf "(doc (source-link %S))" format |> parse |> C.get_doc_options in
  c.source_link span

let get_source_link format span =
  let c = Printf.sprintf "(doc (source-link %S))" format |> parse |> C.get_doc_options in
  c.source_link span

let mk_span file =
  let open Span in
  let buf = Lexing.from_string "a\nb" in
  Lines.using file buf @@ fun l ->
  buf.lex_curr_p <- { buf.lex_curr_p with pos_cnum = 2; pos_lnum = 1 };
  Lines.new_line l;
  let s = { Lexing.pos_bol = 0; pos_lnum = 1; pos_cnum = 0; pos_fname = "" } in
  let f = { Lexing.pos_bol = 0; pos_lnum = 2; pos_cnum = 2; pos_fname = "" } in
  of_span2 (of_pos2 l s s) (of_pos2 l f f)

let span = Span.Filename.mk ~path:Fpath.(root / "test.lua") "=in" |> mk_span

let tests =
  group "Config format"
    [ group "Documentation source links"
        [ mk_alcotest_case "Supports a single line" `Quick (fun () ->
              Alcotest.(check (option string))
                "Format string is correct"
                (get_source_link "[ ${path} ${line} ]" span)
                (Some "[ test.lua 1 ]"));
          mk_alcotest_case "Supports a line range" `Quick (fun () ->
              Alcotest.(check (option string))
                "Format string is correct"
                (get_source_link "[ ${path} ${sline} ${eline} ]" span)
                (Some "[ test.lua 1 2 ]"));
          mk_alcotest_case "Supports git commits" `Quick (fun () ->
              let sha = get_source_link "${commit}" span |> Option.get in
              Alcotest.(check int)
                (Printf.sprintf "SHA is correct length (%s)" sha)
                (String.length sha) 40;
              let is_hex x = (x >= '0' && x <= '9') || (x >= 'a' && x <= 'f') in
              Alcotest.(check (list char))
                (Printf.sprintf "All characters are hex (%s)" sha)
                (String.to_seq sha |> Seq.filter (fun x -> not (is_hex x)) |> List.of_seq)
                []);
          mk_alcotest_case "Path fails for temporary programs" `Quick (fun () ->
              Alcotest.(check (option string))
                "Format string is None"
                (Span.Filename.mk "=in" |> mk_span |> maybe_source_link "[ ${path} ]")
                None);
          mk_alcotest_case "Line works for temporary programs" `Quick (fun () ->
              Alcotest.(check (option string))
                "Format string is correct"
                (Span.Filename.mk "=in" |> mk_span |> maybe_source_link "[ ${line} ]")
                (Some "[ 1 ]"))
        ]
    ]
