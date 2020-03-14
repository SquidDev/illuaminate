open Alcotest
open IlluaminateData

let ref_key = Key.deferred ~name:"Reference" ()

let add_key = Key.key ~name:"Add" (fun s (a, b) -> need s ref_key a + need s ref_key b)

let count_key key k =
  let counter = ref 0 in
  (Key.key ~name:"Count" (fun s () -> incr counter; need s key k), counter)

let ref_builder = Builder.oracle ref_key (fun x _ -> !x)

let test_case t s f = OmnomnomAlcotest.of_alcotest_case (test_case t s f)

let tests =
  Omnomnom.Tests.group "The data/incremental system"
    [ ( test_case "Do not rebuild on constant builds" `Quick @@ fun () ->
        let a = ref 1 and b = ref 2 in
        let changed_key, changed = count_key add_key (a, b) in
        let data = Builder.(empty |> ref_builder |> build) in

        (* An initial build should result in 3 and always change. *)
        check int "Computed value" 3 (get data changed_key ());
        check int "Changed" 1 !changed;

        (* Swap the two: we shouldn't rebuild the final result. *)
        refresh data;
        check int "Computed value" 3 (get data changed_key ());
        check int "Did not change" 1 !changed;

        () );
      ( test_case "Do not rebuild on non-changing builds" `Quick @@ fun () ->
        let a = ref 1 and b = ref 2 in
        let changed_key, changed = count_key add_key (a, b) in
        let data = Builder.(empty |> ref_builder |> build) in

        (* An initial build should result in 3 and always change. *)
        check int "Computed value" 3 (get data changed_key ());
        check int "Changed" 1 !changed;

        (* Swap the two: we shouldn't rebuild the final result. *)
        a := 2;
        b := 1;
        refresh data;
        check int "Computed value" 3 (get data changed_key ());
        check int "Did not change" 1 !changed;

        () );
      ( test_case "Rebuild on non-changing builds" `Quick @@ fun () ->
        let a = ref 1 and b = ref 2 in
        let changed_key, changed = count_key add_key (a, b) in
        let data = Builder.(empty |> ref_builder |> build) in

        (* An initial build should result in 3 and always change. *)
        check int "Computed value" 3 (get data changed_key ());
        check int "Changed" 1 !changed;

        (* Swap the two: we shouldn't rebuild the final result. *)
        a := 2;
        b := 3;
        refresh data;
        check int "Computed value" 5 (get data changed_key ());
        check int "Changed" 2 !changed;

        () )
    ]
