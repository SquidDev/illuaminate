open Lsp_test
open Lsp.Types

let code_actions t ~uri range =
  request t
    (CodeAction { textDocument = { uri }; range; context = { diagnostics = []; only = None } })

let tests =
  Omnomnom.Tests.group "Code actions"
    [ test ~name:"Linter errors have a code action" (fun t ->
          let uri = open_file t "parens.lua" in
          code_actions t ~uri (range 0 1 0 9)
          |> Check.ok_response
          |> Testable.(
               check
                 (code_action_result reject (code_action ~command:(command ~arguments:pass ()) ())))
               "Has an appropriate code action"
               (Some
                  [ `CodeAction
                      (CodeAction.create ~title:"Fix 'Unnecessary parenthesis.'" ~kind:QuickFix
                         ~diagnostics:
                           [ Diagnostic.create ~range:(range 0 6 0 9) ~severity:Hint
                               ~code:(`String "syntax:redundant-parens") ~source:"illuaminate"
                               ~message:"Unnecessary parenthesis." ()
                           ]
                         ~isPreferred:true
                         ~command:
                           (Command.create ~title:"Fix 'Unnecessary parenthesis.'"
                              ~command:"illuaminate/fix" ~arguments:[ `Int 0; `String "" ] ())
                         ())
                  ]);
          ());
      test ~name:"Code actions can be applied" (fun t ->
          let uri = open_file t "parens.lua" in
          let { Command.command; arguments; _ } =
            match code_actions t ~uri (range 0 1 0 9) |> Check.ok_response with
            | Some [ `CodeAction { command = Some command; _ } ] -> command
            | _ -> Alcotest.fail "Code action has no command"
          in
          request t (ExecuteCommand { command; arguments }) |> Check.ok_response |> ignore;
          apply_edits t;
          contents t uri |> Alcotest.(check string) "parens.lua was updated" "print(2)\n";
          ())
    ]
