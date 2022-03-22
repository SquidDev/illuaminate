open IlluaminateDocEmit__.Helpers
open Omnomnom.Tests
open OmnomnomAlcotest

let tests =
  group "Doc summary"
    [ mk_alcotest_case "Sentence with new line" `Quick (fun () ->
          let md = Omd.of_string "This is a short\nsummary." |> get_summary |> Omd.to_plain_text in
          Alcotest.(check string) "Summary is equal" md "This is a short summary.");
      mk_alcotest_case "Summary with a paragraph break." `Quick (fun () ->
          let md = Omd.of_string "This is a short summary\n\nAnd a new paragraph." |> get_summary |> Omd.to_plain_text in
          Alcotest.(check string) "Summary is equal" md "This is a short summary")
    ]
