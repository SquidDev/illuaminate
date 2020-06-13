open Lsp_test
open CCFun
open Leak

let test ~name ?n ?threshold ?workspace ~init actions =
  test ~name ~speed:`Slow ?workspace @@ fun t -> run ?n ?threshold ~init (actions t)

let tests =
  Omnomnom.Tests.group "Memory leaks"
    [ test ~n:1000 ~name:"Open/Close" ~init:() (fun t ->
          action ~name:"Open" (fun () -> open_file t "basic.lua")
          >-> action ~name:"Close" (fun uri -> close_file t uri))
    ]
