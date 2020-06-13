let pp_size out x =
  if x < 0 then Format.fprintf out "-%a" Fmt.bi_byte_size (-x)
  else Format.fprintf out "+%a" Fmt.bi_byte_size x

type ('a, 'b) action =
  | One : string * ('a -> 'b) -> ('a, 'b) action
  | Action : ('a, 'b) action * ('b, 'c) action -> ('a, 'c) action

let action ~name f = One (name, f)

let ( >-> ) f g = Action (f, g)

let rec eval : 'a 'b. ('a, 'b) action -> 'a -> 'b =
 fun f s ->
  match f with
  | One (_, f) -> f s
  | Action (f, g) -> eval f s |> eval g

let rec eval_trace : 'a 'b. ('a, 'b) action -> 'a -> 'b =
 fun f s ->
  match f with
  | One (name, f) ->
      let before = Gc.stat () in
      let result = f s in
      Gc.full_major ();
      let after = Gc.stat () in

      Format.printf "| %30s | %a\n" name pp_size (after.live_words - before.live_words);
      result
  | Action (f, g) -> eval_trace f s |> eval_trace g

let run ?(n = 1000) ?(threshold = 100_000) ~init:state actions =
  (* We run the iteration once - hopefully should get us in a stable state. Then we run n times,
     monitoring memory before and after. If we think we have a leak, then we run a final time,
     tracking how much data is allocated. *)
  let state = eval actions state in

  Gc.full_major ();
  let before = Gc.stat () in

  let rec eval_n n state = if n <= 0 then state else eval actions state |> eval_n (n - 1) in
  let state = eval_n n state in

  Gc.full_major ();
  let after = Gc.stat () in

  let change = after.live_words - before.live_words in
  let msg =
    Fmt.str "Using %a words before, and %a afterwards (change of %a).\n" pp_size before.live_words
      pp_size after.live_words pp_size change
  in

  if change > threshold then (
    eval_trace actions state |> ignore;
    Alcotest.fail msg )
  else Printf.printf "%s\n" msg

let run_unit = run ~init:()
