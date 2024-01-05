(** Generate our [rules.inc] file. *)
let gen_rules () =
  let files =
    Array.to_seq Sys.argv |> Seq.drop 2 |> Seq.map Fpath.v
    |> Seq.filter (Fpath.has_ext "md")
    |> Seq.map (fun x -> Fpath.rem_ext x |> Fpath.to_string)
    |> List.of_seq |> List.sort String.compare
  in
  List.iter
    (fun name ->
      let println x = Printf.printf (x ^^ "\n") in
      println
        {|(rule (deps (glob_files extra/*.lua)) (action (with-stdout-to %s.new (with-stdin-from %%{dep:%s.md} (run %%{dep:./main.exe} md)))))|}
        name name;
      println {|(rule (alias %s) (action (diff %s.md %s.new)))|} name name name)
    files;
  print_string {|
(alias
 (name runtest)
 (deps|};
  List.iter (Printf.printf "\n  (alias %s)") files;
  print_endline "))"

let regex =
  let open Re in
  let code_block lang =
    seq [ bol; str ("```" ^ lang); char '\n'; group (non_greedy (rep any)); str "\n```"; eol ]
  in
  seq
    [ code_block "lua";
      opt (seq [ repn (char '\n') 1 (Some 2); code_block "txt" ]);
      opt (seq [ repn (char '\n') 1 (Some 2); code_block "diff" ]);
      opt (char '\n')
    ]
  |> compile

(** Process a markdown file. *)
let markdown () =
  let input = In_channel.input_all stdin in

  let limit = String.length input in
  let rec go pos =
    if pos > limit then ()
    else
      match Re.exec_opt ~pos regex input with
      | None -> output_substring stdout input pos (limit - pos)
      | Some group ->
          let start, stop = Re.Group.offset group 0 in
          let lua = Re.Group.get group 1 in

          output_substring stdout input pos (start - pos);
          Printf.printf "```lua\n%s\n```\n\n" lua;

          print_endline "```txt";

          (* Print ```txt first to "capture" stdout. *)
          let result =
            (* FIXME: Some tests fail without a trailing new line. *)
            try Ok (Fragment_linter.process ~name:"in.lua" lua)
            with e -> Error (e, Printexc.get_raw_backtrace ())
          in
          (match result with
          | Ok (errs, diff) ->
              (match errs with
              | [] -> print_endline "No errors"
              | _ ->
                  let out = Format.std_formatter in
                  Illuaminate.Console_reporter.display_of_string ~with_summary:false ~out
                    (fun _ -> Some lua)
                    errs;
                  Format.pp_print_flush out ());

              Option.iter
                (fun diff ->
                  print_endline "```\n\n```diff";
                  Format.fprintf Format.std_formatter "%a@\n@?" Diff.pp diff)
                diff
          | Error (err, bt) ->
              Printexc.to_string err |> print_endline;
              Printexc.print_raw_backtrace stdout bt);
          print_endline "```";

          go stop
  in

  go 0

let () =
  Printexc.record_backtrace true;
  match Sys.argv.(1) with
  | "rules" -> gen_rules ()
  | "md" -> markdown ()
  | _ -> exit 1
