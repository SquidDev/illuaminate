open Alcotest
open IlluaminateData

module Ref = struct
  type t =
    { name : string;
      mutable contents : int
    }

  let equal (x : t) (y : t) = x == y
  let hash { name; _ } = Hashtbl.hash name
  let mk name contents = { name; contents }
  let pp out { name; contents } = Format.fprintf out "#%s = %d" name contents
  let v (x : t) = x.contents
end

module RefPair = struct
  type t = Ref.t * Ref.t

  let equal (a1, b1) (a2, b2) = Ref.equal a1 a2 && Ref.equal b1 b2
  let hash (a, b) = (Ref.hash a * 31) + Ref.hash b
  let pp out (a, b) = Format.fprintf out "(%a, %a)" Ref.pp a Ref.pp b
end

let ref_key = Key.deferred ~name:"Reference" ~key:(module Ref) ()

let add_key =
  Key.key ~name:"Add" ~key:(module RefPair) (fun s (a, b) -> need s ref_key a + need s ref_key b)

let count_key key k =
  let counter = ref 0 in
  (Key.key ~name:"Count" ~key:(module Keys.Unit) (fun s () -> incr counter; need s key k), counter)

let ref_builder = Builder.oracle ref_key (fun x _ -> Ref.v x)
let test_case t s f = OmnomnomAlcotest.of_alcotest_case (test_case t s f)

let tests =
  Omnomnom.Tests.group "The data/incremental system"
    [ ( test_case "Do not rebuild on constant builds" `Quick @@ fun () ->
        let a = Ref.mk "a" 1 and b = Ref.mk "b" 2 in
        let changed_key, changed = count_key add_key (a, b) in
        let data = Builder.(build @@ fun b -> ref_builder b) in

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
        let data = Builder.(build @@ fun b -> ref_builder b) in

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
        let data = Builder.(build @@ fun b -> ref_builder b) in

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
