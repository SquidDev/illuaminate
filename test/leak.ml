type ('a, 'b) action =
  | One : string * ('a -> 'b) -> ('a, 'b) action
  | Action : ('a, 'b) action * ('b, 'c) action -> ('a, 'c) action

let action ~name f = One (name, f)

let ( >-> ) f g = Action (f, g)

let pp_size out x =
  if x < 0 then Format.fprintf out "-%a" Fmt.bi_byte_size (-x)
  else Format.fprintf out "+%a" Fmt.bi_byte_size x

let run ?(n = 1000) ?(threshold = 100_000) ~init actions =
  let stats = Hashtbl.create 16 in

  (* Evaluate a action once, and then n times. After each leaf, we update the memory change for that
     action. *)
  let rec eval : 'a 'b. ('a, 'b) action -> Gc.stat * 'a -> Gc.stat * 'b =
   fun f ((before, s) as ss) ->
    match f with
    | One (name, f) ->
        let result = f s in
        Gc.full_major ();
        let after = Gc.stat () in

        let previous = Hashtbl.find_opt stats name |> Option.value ~default:0 in
        previous + (after.live_words - before.live_words) |> Hashtbl.replace stats name;
        (after, result)
    | Action (f, g) -> eval f ss |> eval g
  in
  let rec eval_n n state = if n <= 0 then state else eval actions state |> eval_n (n - 1) in

  Gc.full_major ();
  let before = Gc.stat () in
  let after, _ = eval_n n (before, init) in

  let change = after.live_words - before.live_words in
  let msg =
    Fmt.str "Using %a words before, and %a afterwards (change of %a).\n" pp_size before.live_words
      pp_size after.live_words pp_size change
  in

  (* Dump the memory usage of each pass. *)
  let rec go : 'a 'b. ('a, 'b) action -> unit = function
    | One (name, _) ->
        Hashtbl.find_opt stats name |> Option.value ~default:0
        |> Format.printf "| %30s | %a\n" name pp_size
    | Action (f, g) -> go f; go g
  in
  go actions;

  if change > threshold then Alcotest.fail msg else Printf.printf "%s\n" msg

let run_unit = run ~init:()
