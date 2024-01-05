open! Error

let error_ansi : severity -> Fmt.color = function
  | Error -> `Red
  | Warning -> `Yellow
  | Note -> `Blue

let pp_source_line out get_line position message =
  let start_l, start_c = (Position.start_pos position :> int * int) in
  let finish_l, finish_c = (Position.finish_pos position :> int * int) in
  let line_no = start_l |> string_of_int in

  let line = get_line position in

  let fmt no line =
    let pp_line out () = Fmt.fmt " %*s" out (String.length line_no) no in
    Fmt.styled (`Fg (`Hi `Green)) pp_line out ();
    match line with
    | None -> Format.fprintf out " │@\n"
    | Some line -> Format.fprintf out " │ %t@\n" line
  in
  fmt "" None;
  fmt line_no (Some (fun out -> Fmt.string out (CCString.replace ~sub:"\t" ~by:" " line)));
  let length =
    if finish_l = start_l then finish_c - start_c + 1 else String.length line - start_c + 1
  in
  fmt ""
    (Some
       (fun out ->
         Fmt.string out (String.make (start_c - 1) ' ');
         Fmt.string out (String.make length '^');
         match message with
         | None -> ()
         | Some message -> Fmt.string out " "; message out ()))

let pp_annotation get_line out (pos, msg) = pp_source_line out get_line pos msg

let display_line get_line out { code; severity; position; message; annotations; trailer; tags = _ }
    =
  let pp_pos out () = Fmt.fmt "%s: %a [%s]@\n" out position.file.name message () code in
  Fmt.styled (`Fg (`Hi (error_ansi severity))) pp_pos out ();

  (match annotations with
  | [] -> pp_source_line out get_line position None
  | xs -> Fmt.list ~sep:Fmt.nop (pp_annotation get_line) out xs);
  Option.iter (fun pp -> Fmt.pf out "%a@\n" pp ()) trailer

let summary out xs =
  let errors, warnings =
    List.fold_left
      (fun (errors, warnings) { severity; _ } ->
        match severity with
        | Error -> (errors + 1, warnings)
        | Warning | Note -> (errors, warnings + 1))
      (0, 0) xs
  in
  if errors > 0 then Format.fprintf out "%d errors and %d warnings@\n" errors warnings
  else if warnings > 0 then Format.fprintf out "No errors and %d warnings@\n" warnings
  else ()

let sort_errors = List.sort (fun l r -> Position.compare l.position r.position)

let display_of_files ?(out = Fmt.stderr) ?(with_summary = true) errors =
  let last = ref None in
  let get_channel name : in_channel =
    match !last with
    | Some (last_name, ch) when name = last_name -> ch
    | Some (_, ch) ->
        close_in ch;
        let ch = open_in name in
        last := Some (name, ch);
        ch
    | None ->
        let ch = open_in name in
        last := Some (name, ch);
        ch
  in
  let get_line (pos : Position.t) =
    match pos.file.path with
    | None -> ""
    | Some path -> (
        let ch = get_channel (Fpath.to_string path) in
        let (Position_map.Pos bol) = Position_map.position_bol pos.position_map pos.start in
        seek_in ch bol;
        try input_line ch with End_of_file -> "")
  in
  Fmt.list (display_line get_line) out (sort_errors errors);
  (match !last with
  | Some (_, ch) -> close_in ch
  | _ -> ());
  if with_summary then summary out errors

let display_of_string ?(out = Fmt.stderr) ?(with_summary = true) getter errors =
  let get_line (pos : Position.t) =
    match getter pos.file with
    | None -> ""
    | Some contents ->
        let (Position_map.Pos bol) = Position_map.position_bol pos.position_map pos.start in
        String.sub contents bol
          ((match String.index_from_opt contents bol '\n' with
           | None -> String.length contents
           | Some x -> x)
          - bol)
  in
  Fmt.list (display_line get_line) out (sort_errors errors);
  if with_summary then summary out errors
