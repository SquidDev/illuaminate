module Unit = struct
  type t = unit

  let equal () () = true
  let hash () = 1
  let pp out () = Format.pp_print_string out "()"
end
