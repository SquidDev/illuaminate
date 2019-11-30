open Omnomnom.Tests
open IlluaminatePattern

let test_cases =
  [ (* No separator. *)
    ("a", "a", true);
    ("a", "x/a", true);
    ("a", "x/y/z/a", true);
    ("a", "ab", false);
    ("a", "ba", false);
    ("a", "a/b", true);
    (* Separator at the start. *)
    ("/", "a", true);
    ("/", "a/b/c", true);
    ("/a", "a", true);
    ("/a", "x/a", false);
    (* Separator in the middle *)
    ("a/b", "a", false);
    ("a/b", "a/b", true);
    ("a/b", "a/b/c", true);
    ("a/b", "x/a/b/c", false);
    (* Separator at the end *)
    ("a/", "a/", true);
    ("a/", "a/b", true);
    ("a/", "a/b/c", true);
    ("a/", "x/a/b/c", true);
    ("a/", "c", false);
    (* Separator at the end and middle *)
    ("a/b/", "a/b/", true);
    ("a/b/", "a/b/c", true);
    ("a/b/", "x/a/b/", false);
    ("a/b/", "x/a/b/c", false);
    (* Wildcards *)
    ("*.txt", "foo.txt", true);
    ("*.txt", "a/foo.txt", true);
    ("*.txt", "foo.txts", false);
    ("a/*.txt", "a/foo.txt", true);
    ("a/*.txt", "a/b/foo.txt", false);
    ("a/*.txt", "x/a/foo.txt", false);
    ("**/a/*.txt", "x/a/foo.txt", true)
    (* TODO: ("**/a/*.txt", "a/foo.txt", true) *)
  ]

let tests =
  test_cases
  |> List.map (fun (pat, dir, expected) ->
         test (Printf.sprintf "%S → %S" pat dir) (fun () ->
             let actual = parse pat |> matches dir in
             if actual = expected then result Pass
             else
               let msg =
                 if expected then
                   Printf.sprintf "Expected pattern %S to match %S, but it didn't" pat dir
                 else Printf.sprintf "Expected pattern %S to not match %S, but it did" pat dir
               in
               result
                 ~message:(Some (fun fmt -> Format.pp_print_string fmt msg))
                 (Failed { backtrace = None })))
  |> group "Patterns"
