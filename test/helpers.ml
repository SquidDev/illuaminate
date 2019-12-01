let diff out old_contents new_contents =
  let module Diff = Patience_diff_lib.Patience_diff in
  let same x = Format.fprintf out " %s@\n" x
  and minus x = Format.fprintf out "-%s@\n" x
  and plus x = Format.fprintf out "+%s@\n" x in
  Diff.String.get_hunks
    ~transform:(fun x -> x)
    ~context:3 ?big_enough:None
    ~prev:(String.split_on_char '\n' old_contents |> Array.of_list)
    ~next:(String.split_on_char '\n' new_contents |> Array.of_list)
  |> List.iter (fun (hunk : string Diff.Hunk.t) ->
         Format.fprintf out "%@%@ -%i,%i +%i,%i %@%@@\n" hunk.prev_start hunk.prev_size
           hunk.next_start hunk.next_size;
         hunk.ranges
         |> List.iter (function
              | Diff.Range.Same xs -> xs |> Array.iter (fun (x, _) -> same x)
              | Prev xs -> Array.iter minus xs
              | Next xs -> Array.iter plus xs
              | Replace (prev, next) -> Array.iter minus prev; Array.iter plus next
              | Unified xs -> Array.iter same xs))
