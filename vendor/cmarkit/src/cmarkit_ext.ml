open Cmarkit

let as_single_paragraph x =
  let rec unwrap = function
    | Block.Paragraph (p, _) -> Some (Block.Paragraph.inline p)
    | Block.Blocks ([ x ], _) -> unwrap x
    | _ -> None
  in
  Doc.block x |> unwrap

let inline_text inline =
  match Cmarkit.Inline.to_plain_text~break_on_soft:false inline with
  | [] -> ""
  | x :: xs ->
    let buf = Buffer.create 16 in
    let append = List.iter (Buffer.add_string buf) in
    append x;
    List.iter (fun x -> Buffer.add_char buf ' '; append x) xs;
    Buffer.contents buf

  let block_lines_contents = function
  | [] -> ""
  | x :: xs ->
    let o = Buffer.create 16 in
    Buffer.add_string o (Block_line.to_string x);
    List.iter (fun l -> Buffer.add_char o '\n'; Buffer.add_string o (Block_line.to_string l)) xs;
    Buffer.contents o

  let cprintf c fmt =
    let out = Format.formatter_of_buffer (Cmarkit_renderer.Context.buffer c) in
    Format.pp_set_margin out 1000;
    Format.fprintf out (fmt ^^ "@?")
