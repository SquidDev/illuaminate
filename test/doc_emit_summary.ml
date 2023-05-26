open IlluaminateDocEmit__.Helpers
open Alcotest

let () =
  let tests =
    [ test_case "Sentence with new line" `Quick (fun () ->
          let md = Omd.of_string "This is a short\nsummary." |> get_summary |> Omd.to_plain_text in
          Alcotest.(check string) "Summary is equal" md "This is a short summary.");
      test_case "Summary with a paragraph break." `Quick (fun () ->
          let md =
            Omd.of_string "This is a short summary\n\nAnd a new paragraph."
            |> get_summary |> Omd.to_plain_text
          in
          Alcotest.(check string) "Summary is equal" md "This is a short summary")
    ]
  in
  Alcotest.run "Doc summary" [ ("Tests", tests) ]
