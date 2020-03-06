let diff out old_contents new_contents =
  let open OmnomnomGolden__.Diff in
  diff
    (String.split_on_char '\n' old_contents |> Array.of_list |> Slice.of_array)
    (String.split_on_char '\n' new_contents |> Array.of_list |> Slice.of_array)
  |> pp_diff out
