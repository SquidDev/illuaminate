(** A shared library for our Markdown-style tests. *)

(** [run action] parses a Markdown file on stdin and runs [action] against each Lua code block,
    spitting out a new markdown file with the output after each Lua block.*)
let run action =
  Printexc.record_backtrace true;
  (* Our parser really just splits by lines and then finds code blocks by looking for [```lua]. *)
  let rec go : string list -> unit = function
    | [] -> ()
    | x :: xs -> (
        print_endline x;
        match CCString.chop_prefix ~pre:"```lua" x with
        | Some prefix -> go_lua prefix xs
        | None -> go xs)
  and go_lua prefix xs =
    let contents, xs = CCList.take_drop_while (fun x -> x <> "```") xs in
    let xs = List.tl xs in
    (* Drop the ``` *)
    let contents = String.concat "\n" contents in
    print_endline contents;
    print_endline "```";
    print_endline "";
    print_endline "```txt";
    action Format.std_formatter prefix contents;
    Format.pp_print_flush Format.std_formatter ();
    print_endline "```";
    go_txt 0 xs
  and go_txt lines = function
    | "" :: xs -> go_txt (lines + 1) xs
    | "```txt" :: xs -> CCList.drop_while (fun x -> x <> "```") xs |> List.tl |> go
    | xs ->
        for _ = 1 to lines do
          print_char '\n'
        done;
        go xs
  in
  In_channel.input_all stdin |> String.trim |> String.split_on_char '\n' |> go
