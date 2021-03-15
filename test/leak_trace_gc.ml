module LList = struct
  type 'a cell =
    { value : 'a;
      mutable prev : 'a cell option;
      mutable next : 'a cell option
    }

  type 'a t =
    { mutable first : 'a cell option;
      mutable last : 'a cell option
    }

  let add t value =
    let cell = { value; prev = t.last; next = None } in
    let s_cell = Some cell in
    (match t.last with
    | None -> t.first <- s_cell
    | Some last -> last.next <- s_cell);
    t.last <- s_cell;
    cell

  let remove t cell =
    (match cell.prev with
    | None -> t.first <- cell.next
    | Some c -> c.next <- cell.next);
    match cell.next with
    | None -> t.last <- cell.prev
    | Some c -> c.prev <- cell.prev

  let rec cell_to_seq cell () : 'a Seq.node =
    let { value; next; _ } = cell in
    let next =
      match next with
      | None -> Seq.empty
      | Some x -> cell_to_seq x
    in
    Cons (value, next)

  let to_seq t () : 'a Seq.node =
    match t.first with
    | None -> Nil
    | Some x -> cell_to_seq x ()

  let make () = { first = None; last = None }

  let clear t =
    t.first <- None;
    t.last <- None
end

type allocation =
  { n_samples : int;
    size : int;
    callstack : Printexc.raw_backtrace
  }

let working = ref false

let tracking = ref true

let allocations = LList.make ()

let allocate (allocation : Gc.Memprof.allocation) =
  if !tracking then
    let allocation =
      { n_samples = allocation.n_samples; size = allocation.size; callstack = allocation.callstack }
    in
    Some (LList.add allocations allocation)
  else None

let deallocate x = LList.remove allocations x

let pause () = tracking := false

let resume () = tracking := true

let run f =
  if !working then failwith "Already sampling";
  LList.clear allocations;
  working := true;
  tracking := true;

  Gc.Memprof.start ~sampling_rate:0.3
    { alloc_minor = allocate;
      alloc_major = allocate;
      promote = Option.some;
      dealloc_minor = deallocate;
      dealloc_major = deallocate
    };
  let res = try Ok (f ()) with e -> Error (e, Printexc.get_raw_backtrace ()) in
  Gc.Memprof.stop ();
  working := false;
  match res with
  | Ok x -> (x, LList.to_seq allocations |> List.of_seq)
  | Error (e, bt) -> Printexc.raise_with_backtrace e bt

let format_bt ?(n = 10) bt =
  Printexc.backtrace_slots bt
  |> Option.fold ~none:Seq.empty ~some:Array.to_seq
  |> Seq.filter_map (fun slot ->
         match (Printexc.Slot.name slot, Printexc.Slot.location slot) with
         | None, Some { filename; line_number; _ } ->
             Printf.sprintf "%s:%d" filename line_number |> Option.some
         | Some n, Some { filename; line_number; _ } ->
             Printf.sprintf "%s:%d (%s)" filename line_number n |> Option.some
         | _, None -> None)
  |> CCSeq.take n |> List.of_seq |> String.concat "\n"
