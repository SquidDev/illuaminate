open IlluaminateCore
open Syntax
open Lens
open IlluaminateLsp__Lsp_convert
module L = IlluaminateLsp__Locate

let report { Span.span; value } =
  let errors = Error.make () in
  IlluaminateParser.Error.report errors span value;
  Alcotest.failf "%a" (fun out x -> Error.display_of_files ~out x) errors

let node : L.node Alcotest.testable =
  ( module struct
    type t = L.node

    let pp = L.pp_node

    let equal = ( = )
  end )

let run path =
  let name = Fpath.basename path in
  OmnomnomAlcotest.mk_alcotest_case name `Quick @@ fun () ->
  let name = Span.Filename.mk ~path name in
  let program =
    CCIO.with_in (Fpath.to_string path) (IlluaminateParser.program name % Lexing.from_channel)
    |> Result.fold ~ok:Fun.id ~error:report
  in

  let locate p = L.locate p program in
  let locate_s p = Node.span p |> span_start |> locate in
  let locate_f p = Node.span p |> span_finish |> locate in
  let iter =
    object
      inherit iter as super

      method! var x =
        locate_f (x ^. Last.var) |> Alcotest.check node "Located correct var" (Var x);
        super#var x

      method! arg x =
        ( match x with
        | NamedArg _ -> ()
        | DotArg x -> locate_f x |> Alcotest.check node "Located correct argument" (DotArg x) );
        super#arg x

      method! function_name x =
        ( match x with
        | FVar _ -> ()
        | _ ->
            locate_f (x ^. Last.function_name)
            |> Alcotest.check node "Located correct function name" (FunctionName x) );
        super#function_name x

      method! name x =
        ( match x with
        | NVar _ -> ()
        | _ -> locate_f (x ^. Last.name) |> Alcotest.check node "Located correct name" (Name x) );
        super#name x

      method! expr x =
        ( match x with
        | Parens _ | Dots _ | Nil _ | True _ | False _ | Int _ | Number _ | MalformedNumber _
        | Table _ | String _ ->
            locate_s (x ^. First.expr) |> Alcotest.check node "Located from opening expr" (Expr x);
            locate_f (x ^. Last.expr) |> Alcotest.check node "Located from closing expr" (Expr x)
        | _ -> () );
        super#expr x

      method! stmt x =
        ( match x with
        | Assign _ -> ()
        | SCall _ ->
            locate_f (x ^. Last.stmt)
            |> Alcotest.check node "Located from closing call stmt" (Stmt x)
        | Local _ | Repeat _ | Return _ ->
            locate_s (x ^. First.stmt) |> Alcotest.check node "Located from opening stmt" (Stmt x)
        | _ ->
            locate_s (x ^. First.stmt) |> Alcotest.check node "Located from opening stmt" (Stmt x);
            locate_f (x ^. Last.stmt) |> Alcotest.check node "Located from closing stmt" (Stmt x) );
        super#stmt x
    end
  in
  iter#program program

let tests =
  let root = Fpath.(v (Sys.getcwd ()) / "data" / "lsp" / "locate") in
  Sys.readdir (Fpath.to_string root)
  |> Array.to_list
  |> List.map (fun x -> Fpath.(root / x))
  |> List.filter (fun x -> Fpath.has_ext ".lua" x)
  |> List.map run |> Omnomnom.Tests.group "Locate"
