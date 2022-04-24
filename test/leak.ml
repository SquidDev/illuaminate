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

let eval_n ~actions ~eval ~n =
  let rec go n state = if n <= 0 then state else eval actions state |> go (n - 1) in
  go n

let rec eval_trace : 'a 'b. ('a, 'b) action -> 'a -> 'b =
 fun f s ->
  match f with
  | One (name, f) ->
      let before = Gc.stat () in
      Leak_trace.resume ();
      let result = f s in
      Gc.full_major ();
      Leak_trace.pause ();
      let after = Gc.stat () in

      Format.printf "| %30s | %a\n" name pp_size (after.live_words - before.live_words);
      result
  | Action (f, g) -> eval_trace f s |> eval_trace g

let print_allocs allocations =
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun { Leak_trace.n_samples; size; callstack; _ } ->
      match Printexc.backtrace_slots callstack with
      | None -> ()
      | Some bt ->
          let existing = Hashtbl.find_opt tbl bt |> Option.fold ~none:[] ~some:snd in
          Hashtbl.replace tbl bt (callstack, (n_samples, size) :: existing))
    allocations;

  let sorted =
    Hashtbl.to_seq_values tbl
    |> Seq.map (fun (bt, v) ->
           let samples = List.fold_left (fun a (samples, _size) -> a + samples) 0 v in
           (Leak_trace.format_bt bt, samples))
    |> List.of_seq
    |> List.sort (fun (_, a) (_, b) -> Int.compare b a)
    |> CCList.take 10
  in
  List.iter (fun (bt, n) -> Printf.printf "%70s\n=>>> %d <<<=\n\n" bt n) sorted

let run ?error ?(n = 1000) ?(threshold = 100_000) ~init:state actions =
  (* We run the iteration once - hopefully should get us in a stable state. Then we run n times,
     monitoring memory before and after. If we think we have a leak, then we run a final time,
     tracking how much data is allocated. *)
  let state = eval actions state in

  Gc.full_major ();
  let before = Gc.stat () in

  let state = eval_n ~eval ~actions ~n state in
  Gc.full_major ();
  let after = Gc.stat () in

  let change = after.live_words - before.live_words in
  let msg =
    Fmt.str "Using %a words before, and %a afterwards (change of %a).\n" pp_size before.live_words
      pp_size after.live_words pp_size change
  in

  if change > threshold then (
    Option.iter (fun f -> Printf.printf "== Dump ==\n"; f state) error;
    Printf.printf "== Trace ==\n";
    let _, allocs =
      Leak_trace.run @@ fun () ->
      Leak_trace.pause ();
      eval_n ~eval:eval_trace ~actions ~n:5 state
    in
    if not (CCList.is_empty allocs) then (
      Printf.printf "== Allocations ==\n";
      print_allocs allocs);
    Alcotest.fail msg)
  else Printf.printf "%s\n" msg

let run_unit = run ~init:()
