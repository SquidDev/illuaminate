type t = { write : string -> int -> int -> unit } [@@unboxed]
type 'a pp = t -> 'a -> unit

let with_output_stream oc fn =
  let buffer = Buffer.create 8192 in
  let write str start len =
    Buffer.add_substring buffer str start len;
    if Buffer.length buffer >= 8192 then (Buffer.output_buffer oc buffer; Buffer.clear buffer)
  in
  Fun.protect ~finally:(fun () -> Buffer.output_buffer oc buffer) @@ fun () -> fn { write }

let of_buffer buffer = { write = Buffer.add_substring buffer }

let of_formatter fmt =
  let write str start len =
    let str = if start = 0 && len = String.length str then str else String.sub str 0 len in
    Format.pp_print_string fmt str
  in
  { write }

let with_to_str fn =
  let b = Buffer.create 16 in
  fn (of_buffer b);
  Buffer.contents b

let write_substring w = w.write
let write w str = w.write str 0 (String.length str)
let printf p fmt = Printf.ksprintf (write p) fmt
