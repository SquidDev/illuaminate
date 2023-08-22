open IlluaminateDocEmit__.Helpers
open IlluaminateSemantics.Doc.Syntax.Markdown
open Alcotest

let () =
  let parse x = Markdown (Cmarkit.Doc.of_string x) in
  let tests =
    [ test_case "Sentence with new line" `Quick (fun () ->
          let md = parse "This is a short\nsummary." |> get_summary |> Cmarkit_ext.inline_text in
          Alcotest.(check string) "Summary is equal" md "This is a short summary.");
      test_case "Summary with a paragraph break." `Quick (fun () ->
          let md =
            parse "This is a short summary\n\nAnd a new paragraph."
            |> get_summary |> Cmarkit_ext.inline_text
          in
          Alcotest.(check string) "Summary is equal" md "This is a short summary")
    ]
  in
  Alcotest.run "Doc summary" [ ("Tests", tests) ]
