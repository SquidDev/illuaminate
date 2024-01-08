include Array

let empty = [||]
let pp fmt = Fmt.Dump.array fmt

external of_array : 'a array -> 'a t = "%identity"

let of_list = Array.of_list

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

let set array idx value =
  let array = Array.copy array in
  array.(idx) <- value;
  array
