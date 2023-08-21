(* A very basic diffing algorithm. We really should optimise this and support displaying partial
   diffs. *)

module Slice = struct
  type 'a t =
    { array : 'a array;
      start : int;
      length : int
    }

  let of_array array = { array; start = 0; length = Array.length array }
  let of_list xs = of_array (Array.of_list xs)
  let length x = x.length

  let slice ~from ~length { array; start; length = old_length } =
    if from < 0 then invalid_arg (Printf.sprintf "from is negative (%d)" from);
    if from + length > old_length then
      invalid_arg
        (Printf.sprintf "range is out of bounds (from=%d + length=%d is greater than %d)" from
           length old_length);
    { array; start = start + from; length }

  let drop n { array; start; length } =
    if n > length then invalid_arg (Printf.sprintf "drop %d on slice of length %d" n length);
    { array; start = start + n; length = length - n }

  let take n { array; start; length } =
    if n > length then invalid_arg (Printf.sprintf "take %d on slice of length %d" n length);
    { array; start; length = n }

  let iteri f { array; start; length } =
    for i = 0 to length - 1 do
      f i array.(start + i)
    done
end

type change =
  [ `Same
  | `Remove
  | `Add
  ]

type t = (change * string Slice.t) list

let add kind slice out = if Slice.length slice = 0 then out else (kind, slice) :: out

let rec diff_ olds news out : t =
  match (Slice.length olds, Slice.length news) with
  | 0, 0 -> out
  | 0, _ -> add `Add news out
  | _, 0 -> add `Remove olds out
  | _, _ ->
      (* Build up a map of all lines contents to their positions. *)
      let old_index_map = Hashtbl.create (Slice.length olds / 2) in
      Slice.iteri (fun i x -> Hashtbl.add old_index_map x i) olds;

      let overlap = Array.make (Slice.length olds) 0 in
      let sub_length = ref 0 and sub_range = ref (0, 0) in

      news
      |> Slice.iteri (fun new_index value ->
             let this_overlap = Array.make (Slice.length olds) 0 in
             Hashtbl.find_all old_index_map value
             |> List.iter (fun old_index ->
                    let this = if old_index = 0 then 1 else overlap.(old_index - 1) + 1 in
                    this_overlap.(old_index) <- this;
                    if this > !sub_length then (
                      sub_length := this;
                      sub_range := (old_index - this + 1, new_index - this + 1)));
             Array.blit this_overlap 0 overlap 0 (Array.length this_overlap));

      if !sub_length = 0 then add `Remove olds @@ add `Add news out
      else
        let old_idx, new_idx = !sub_range and length = !sub_length in
        let out =
          (* Right*)
          diff_
            (Slice.slice ~from:(old_idx + length)
               ~length:(Slice.length olds - old_idx - length)
               olds)
            (Slice.slice ~from:(new_idx + length)
               ~length:(Slice.length news - new_idx - length)
               news)
            out
        in
        let out = add `Same (Slice.slice ~from:new_idx ~length news) out in
        let out =
          (* Left *)
          diff_
            (Slice.slice ~from:0 ~length:old_idx olds)
            (Slice.slice ~from:0 ~length:new_idx news)
            out
        in
        out

let diff x y = diff_ (Slice.of_list x) (Slice.of_list y) []

module PP = struct
  let colour = function
    | `Same -> `White
    | `Remove -> `Hi `Red
    | `Add -> `Hi `Green

  let get_prefix : change -> string = function
    | `Same -> " "
    | `Remove -> "-"
    | `Add -> "+"

  let pp_line out change line =
    match (change, line) with
    | `Same, "" -> ()
    | _, "" -> Format.pp_print_string out (get_prefix change)
    | _ ->
        Format.pp_print_string out (get_prefix change);
        Format.pp_print_string out " ";
        Fmt.styled (`Fg (colour change)) Fmt.string out line

  let pp_hunk out ~cut ~alo ~ahi ~blo ~bhi parts =
    let pp_header out () = Format.fprintf out "@@ -%d,%d +%d,%d @@" alo ahi blo bhi in
    if cut then Format.pp_print_space out ();
    Fmt.styled (`Fg `Magenta) pp_header out ();

    Fun.flip List.iter parts @@ fun (kind, slice) ->
    Slice.iteri (fun _ x -> Format.pp_print_space out (); pp_line out kind x) slice

  let context = 3

  let pp_trailing out ~cut ~acc ~alo ~ahi ~blo ~bhi (lines : _ Slice.t) =
    let stop = Int.min context (Slice.length lines) in
    let ahi = ahi + stop and bhi = bhi + stop in
    let acc = (`Same, Slice.take stop lines) :: acc in
    pp_hunk out ~cut ~alo ~ahi ~blo ~bhi (List.rev acc)

  let pp_patch out (lines : t) =
    let rec go ~cut ~acc ~alo ~ahi ~blo ~bhi : t -> unit = function
      (* We finished on a Add/Remove, just display the hunk. *)
      | [] -> pp_hunk out ~cut ~alo ~ahi ~blo ~bhi (List.rev acc)
      (* We finished on an equal block. Take the first n lines and display the hunk. *)
      | [ (`Same, lines) ] -> pp_trailing out ~cut ~acc ~alo ~ahi ~blo ~bhi lines
      (* If we're a Same block, either finish a hunk or just add some context to the existing
         one. *)
      | (`Same, lines) :: xs ->
          let length = Slice.length lines in
          if length > context * 2 then (
            pp_trailing out ~cut ~acc ~alo ~ahi ~blo ~bhi lines;

            let alo = ahi + length + (2 * context) in
            let blo = bhi + length + (2 * context) in
            let acc = [ (`Same, Slice.slice ~from:(length - context) ~length:context lines) ] in
            go ~cut:true ~acc ~alo ~ahi:alo ~blo ~bhi:blo xs)
          else
            let acc = (`Same, lines) :: acc in
            go ~cut ~acc ~alo ~ahi:(ahi + length) ~blo ~bhi:(bhi + length) xs
      | (kind, lines) :: xs ->
          let length = Slice.length lines in
          let ahi, bhi =
            match kind with
            | `Add -> (ahi + length, bhi)
            | `Remove -> (ahi, bhi + length)
            | _ -> assert false (* Can't be `Same *)
          in
          let acc = (kind, lines) :: acc in
          go ~cut ~acc ~alo ~ahi ~blo ~bhi xs
    in
    match lines with
    | [ (`Same, _) ] -> ()
    | (`Same, lines) :: xs ->
        let length = Slice.length lines in
        let start = max 0 (length - context) in
        let acc = [ (`Same, Slice.drop start lines) ] in
        go ~cut:false ~acc ~alo:length ~ahi:length ~blo:length ~bhi:length xs
    | lines -> go ~cut:false ~acc:[] ~alo:1 ~ahi:1 ~blo:1 ~bhi:1 lines
end

let diff ~old ~new_ : t =
  if old = new_ then
    let lines =
      match String.split_on_char '\n' old with
      | [] -> []
      | xs -> [ (`Same, Slice.of_list xs) ]
    in
    lines
  else diff (String.split_on_char '\n' old) (String.split_on_char '\n' new_)

let pp out = Format.fprintf out "@[<v>%a@]" PP.pp_patch
