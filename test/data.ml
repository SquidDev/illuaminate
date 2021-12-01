open Alcotest
open IlluaminateData

module Ref = struct
  type 'a t =
    { name : string;
      mutable contents : 'a
    }

  let hash { name; _ } = Hashtbl.hash name

  let mk name contents = { name; contents }

  let pp f out { name; contents } = Format.fprintf out "#%s = %a" name f contents

  let v (x : _ t) = x.contents
end

let ref_key =
  Key.deferred ~name:"Reference" ~pp:(Ref.pp Fmt.int)
    ~container:(Container.strong ~hash:Ref.hash ~eq:( == ) ())
    ()

let add_key =
  Key.key ~name:"Add"
    ~pp:Fmt.(pair ~sep:comma (Ref.pp int) (Ref.pp int))
    (fun s (a, b) -> need s ref_key a + need s ref_key b)

let count_key key k =
  let counter = ref 0 in
  (Key.key ~name:"Count" ~pp:(Fmt.any "") (fun s () -> incr counter; need s key k), counter)

let ref_builder = Builder.oracle ref_key (fun x _ -> Ref.v x)

let test_case t s f = OmnomnomAlcotest.of_alcotest_case (test_case t s f)

let tests =
  Omnomnom.Tests.group "The data/incremental system"
    [ ( test_case "Do not rebuild on constant builds" `Quick @@ fun () ->
        let a = Ref.mk "a" 1 and b = Ref.mk "b" 2 in
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
        let a = Ref.mk "a" 1 and b = Ref.mk "b" 2 in
        let changed_key, changed = count_key add_key (a, b) in
        let data = Builder.(empty |> ref_builder |> build) in

        (* An initial build should result in 3 and always change. *)
        check int "Computed value" 3 (get data changed_key ());
        check int "Changed" 1 !changed;

        (* Swap the two: we shouldn't rebuild the final result. *)
        a.contents <- 2;
        b.contents <- 1;
        refresh data;
        check int "Computed value" 3 (get data changed_key ());
        check int "Did not change" 1 !changed;

        () );
      ( test_case "Rebuild on non-changing builds" `Quick @@ fun () ->
        let a = Ref.mk "a" 1 and b = Ref.mk "b" 2 in
        let changed_key, changed = count_key add_key (a, b) in
        let data = Builder.(empty |> ref_builder |> build) in

        (* An initial build should result in 3 and always change. *)
        check int "Computed value" 3 (get data changed_key ());
        check int "Changed" 1 !changed;

        (* Swap the two: we shouldn't rebuild the final result. *)
        a.contents <- 2;
        b.contents <- 3;
        refresh data;
        check int "Computed value" 5 (get data changed_key ());
        check int "Changed" 2 !changed;

        () )
    ]
