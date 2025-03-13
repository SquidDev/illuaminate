include Array

let empty = [||]
let pp fmt = Fmt.Dump.array fmt
let singleton x = [| x |]

external of_array : 'a array -> 'a t = "%identity"

let of_rev_list = function
  | [] -> empty
  | hd :: _ as l ->
      let len = List.length l in
      let a = Array.make len hd in
      let rec fill i = function
        | [] -> a
        | hd :: tl ->
            unsafe_set a i hd;
            fill (i - 1) tl
      in
      fill (len - 1) l

let is_empty xs = length xs = 0
let first xs = xs.(0)
let last xs = xs.(length xs - 1)

let rec filter_worker acc f xs i =
  if i >= length xs then of_rev_list acc
  else
    let elem = xs.(i) in
    let acc = if f elem then elem :: acc else acc in
    filter_worker acc f xs (i + 1)

let filter f xs = filter_worker [] f xs 0

let set array idx value =
  let array = Array.copy array in
  array.(idx) <- value;
  array

let push_first x xs = Array.append [| x |] xs
let push_last xs x = Array.append xs [| x |]
