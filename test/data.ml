open Omnomnom.Tests
open IlluaminateData

let ref_key = Key.deferred ~name:"Reference" ()

let add_key = Key.key ~name:"Add" (fun s (a, b) -> need s ref_key a + need s ref_key b)

let count_key key k =
  let counter = ref 0 in
  (Key.key ~name:"Count" (fun s () -> incr counter; need s key k), counter)

let ref_builder = Builder.oracle ref_key (fun x _ -> !x)

let ( let+ ) x f =
  match x with
  | { outcome = Pass; _ } -> f ()
  | x -> x

let assert_eq ~pp exp actual =
  if exp == actual then result Pass
  else
    result
      ~message:(fun f -> Format.fprintf f "Expected %a, got %a" pp exp pp actual)
      (Failed { backtrace = Some (Printexc.get_raw_backtrace ()) })

let pp_int f = Format.fprintf f "%d"

let tests =
  group "The data/incremental system"
    [ ( test "Do not rebuild on constant builds" @@ fun () ->
        let a = ref 1 and b = ref 2 in
        let changed_key, changed = count_key add_key (a, b) in
        let data = Builder.(empty |> ref_builder |> build) in

        (* An initial build should result in 3 and always change. *)
        let+ () = assert_eq ~pp:pp_int 3 (get data changed_key ()) in
        let+ () = assert_eq ~pp:pp_int 1 !changed in

        (* Swap the two: we shouldn't rebuild the final result. *)
        refresh data;
        let+ () = assert_eq ~pp:pp_int 3 (get data changed_key ()) in
        let+ () = assert_eq ~pp:pp_int 1 !changed in

        result Pass );
      ( test "Do not rebuild on non-changing builds" @@ fun () ->
        let a = ref 1 and b = ref 2 in
        let changed_key, changed = count_key add_key (a, b) in
        let data = Builder.(empty |> ref_builder |> build) in

        (* An initial build should result in 3 and always change. *)
        let+ () = assert_eq ~pp:pp_int 3 (get data changed_key ()) in
        let+ () = assert_eq ~pp:pp_int 1 !changed in

        (* Swap the two: we shouldn't rebuild the final result. *)
        a := 2;
        b := 1;
        refresh data;
        let+ () = assert_eq ~pp:pp_int 3 (get data changed_key ()) in
        let+ () = assert_eq ~pp:pp_int 1 !changed in

        result Pass );
      ( test "Rebuild on non-changing builds" @@ fun () ->
        let a = ref 1 and b = ref 2 in
        let changed_key, changed = count_key add_key (a, b) in
        let data = Builder.(empty |> ref_builder |> build) in

        (* An initial build should result in 3 and always change. *)
        let+ () = assert_eq ~pp:pp_int 3 (get data changed_key ()) in
        let+ () = assert_eq ~pp:pp_int 1 !changed in

        (* Swap the two: we shouldn't rebuild the final result. *)
        a := 2;
        b := 3;
        refresh data;
        let+ () = assert_eq ~pp:pp_int 5 (get data changed_key ()) in
        let+ () = assert_eq ~pp:pp_int 2 !changed in

        result Pass )
    ]
