open OpamParserTypes.FullPos
module J = Yojson.Safe

let rec json_of_value value : J.t =
  match value.pelem with
  | String value | Ident value -> `String value
  | Bool value -> `Bool value
  | Int value -> `Int value
  | List xs -> `List (List.map json_of_value xs.pelem)
  | Pfxop _ | Option _ | Relop _ | Prefix_relop _ | Group _ | Logop _ | Env_binding _ ->
      `String (OpamPrinter.FullPos.value value)

type dep_kind =
  | Main
  | Dev

let dep_of_value value : dep_kind * string * string =
  match value.pelem with
  | String p -> (Main, p, "*")
  | Option ({ pelem = String p; _ }, constraints) ->
      let rec flat xs = function
        | { pelem = Logop ({ pelem = `And; _ }, l, r); _ } -> flat (flat xs l) r
        | value -> value :: xs
      in
      let rec build (kind, version) = function
        | [] -> (kind, version)
        | { pelem = Ident ("with-test" | "with-doc" | "build" | "dev"); _ } :: xs ->
            build (Dev, version) xs
        | { pelem = Prefix_relop (op, { pelem = String v; _ }); _ } :: xs ->
            let this = OpamPrinter.FullPos.relop op ^ v in
            let version = Option.fold ~some:(Printf.sprintf "%s %s" this) ~none:this version in
            build (kind, Some version) xs
        | c :: _ ->
            Printf.sprintf "Unknown constraint '%s' for '%s'" (OpamPrinter.FullPos.value c) p
            |> failwith
      in
      let kind, version = List.fold_left flat [] constraints.pelem |> build (Main, None) in
      (kind, p, Option.value ~default:"*" version)
  | _ -> Printf.sprintf "Unknown package '%s'" (OpamPrinter.FullPos.value value) |> failwith

let to_json fields item : (string * J.t) list =
  let add_field k v = (k, json_of_value v) :: fields in
  match item.pelem with
  | Variable ({ pelem = ("version" | "license" | "homepage") as k; _ }, v) -> add_field k v
  | Variable ({ pelem = "synopsis"; _ }, value) -> add_field "description" value
  | Variable ({ pelem = "depends"; _ }, { pelem = List depends; _ }) ->
      let add (main, dev) v =
        let kind, name, version = dep_of_value v in
        let name =
          match name with
          | "ocaml" -> name
          | _ -> "@opam/" ^ name
        in
        let dep = (name, `String version) in
        match kind with
        | Main -> (dep :: main, dev)
        | Dev -> (main, dep :: dev)
      in
      let main, dev = List.fold_left add ([], []) depends.pelem in
      ("devDependencies", `Assoc (List.rev dev))
      :: ("dependencies", `Assoc (List.rev main))
      :: fields
  | Variable ({ pelem = "pin-depends"; _ }, { pelem = List depends; _ }) ->
      let pins =
        List.map
          (function
            | { pelem =
                  List { pelem = [ { pelem = String name; _ }; { pelem = String package; _ } ]; _ };
                _
              } ->
                let name = CCString.chop_suffix ~suf:".dev" name |> Option.value ~default:name in
                let package =
                  match String.split_on_char '#' package with
                  | [ l; r ] -> Printf.sprintf "%s:%s.opam#%s" l name r
                  | _ -> package
                in

                ("@opam/" ^ name, `String package)
            | pin -> Printf.sprintf "Unknown pin %s" (OpamPrinter.FullPos.value pin) |> failwith)
          depends.pelem
      in

      ("resolutions", `Assoc (List.rev pins)) :: fields
  | _ -> fields

let () =
  let { file_contents; _ } =
    match Sys.argv with
    | [| _; x |] -> OpamParser.FullPos.file x
    | [| _ |] -> OpamParser.FullPos.channel stdin "=stdin"
    | _ -> Printf.eprintf "esy.exe INPUT\n"; exit 1
  in
  let fields = List.fold_left to_json [] file_contents in
  let json : J.t =
    `Assoc
      ((("name", `String "illuaminate") :: List.rev fields)
      @ [ ( "esy",
            `Assoc
              [ ("build", `String "dune build -p #{self.name}");
                ("release", `Assoc [ ("includePackages", `List [ `String "root" ]) ]);
                ("buildEnv", `Assoc [ ("PATH", `String "#{self.root / '_build_tools' : $PATH}") ])
              ] );
          ( "scripts",
            `Assoc
              [ ("test", `String "dune build @runtest -f");
                ("format", `String "dune build @fmt --auto-promote")
              ] )
        ])
  in
  J.pretty_to_channel ~std:true stdout json;
  print_newline ()
