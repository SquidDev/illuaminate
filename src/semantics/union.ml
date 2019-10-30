type 'a t = 'a point ref

and 'a point =
  | Info of
      { value : 'a;
        weight : int
      }
  | Link of 'a t

let make value = ref (Info { value; weight = 1 })

let rec unwrap (point : 'a t) : 'a t =
  match !point with
  | Link x ->
      let x' = unwrap x in
      if x != x' then point := x'.contents;
      x'
  | Info _ -> point

let get point =
  match !point with
  | Info { value; _ } -> value
  | Link _ -> (
    match !(unwrap point) with
    | Info { value; _ } -> value
    | _ -> failwith "Impossible" )

let set point value =
  let point = unwrap point in
  match !point with
  | Info { weight; _ } -> point := Info { weight; value }
  | Link _ -> failwith "Impossible"

let union_with f left right =
  let left = unwrap left and right = unwrap right in
  if left != right then
    match (!left, !right) with
    | Info l, Info r ->
        let info = Info { value = f l.value r.value; weight = l.weight + r.weight } in
        if l.weight >= r.weight then (
          left := info;
          right := Link left )
        else (
          right := info;
          left := Link right )
    | _, _ -> failwith "Impossible"

let union l r = union_with (fun _ x -> x) l r
