open OpamParserTypes.FullPos
module J = Yojson.Safe

module Versions = struct
  let omnomnom = "33781a0000353d0d4b7ae444b33c7288ae5e5011"

  let lsp = "ea9e7f6f9e2e402d55c7ec0de56fb9a2f58d3e58"
end

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
  | Skip

let dep_of_value ~os value : dep_kind * string * string =
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
        | { pelem =
              Relop
                ( { pelem = `Neq; _ },
                  { pelem = Ident "os-family"; _ },
                  { pelem = String this_os; _ } );
            _
          }
          :: xs ->
            if this_os <> os then build (kind, version) xs else (Skip, version)
        | c :: _ ->
            Printf.sprintf "Unknown constraint '%s' for '%s'" (OpamPrinter.FullPos.value c) p
            |> failwith
      in
      let kind, version = List.fold_left flat [] constraints.pelem |> build (Main, None) in
      (kind, p, Option.value ~default:"*" version)
  | _ -> Printf.sprintf "Unknown package '%s'" (OpamPrinter.FullPos.value value) |> failwith

let to_json ~os fields item : (string * J.t) list =
  let add_field k v = (k, json_of_value v) :: fields in
  match item.pelem with
  | Variable ({ pelem = ("version" | "license" | "homepage") as k; _ }, v) -> add_field k v
  | Variable ({ pelem = "synopsis"; _ }, value) -> add_field "description" value
  | Variable ({ pelem = "depends"; _ }, { pelem = List depends; _ }) ->
      let add (main, dev) v =
        let kind, name, version = dep_of_value ~os v in
        let name =
          match name with
          | "ocaml" -> name
          | _ -> "@opam/" ^ name
        in
        let dep = (name, `String version) in
        match kind with
        | Main -> (dep :: main, dev)
        | Dev -> (main, dep :: dev)
        | Skip -> (main, dev)
      in
      let main, dev = List.fold_left add ([], []) depends.pelem in
      ("devDependencies", `Assoc (List.rev dev))
      :: ("dependencies", `Assoc (List.rev main)) :: fields
  | _ -> fields

let () =
  let os =
    match Sys.os_type |> String.lowercase_ascii with
    | "win32" | "cygwin" -> "windows"
    | "freebsd" | "openbsd" | "netbsd" | "dragonfly" -> "bsd"
    | x -> x
  in
  let os = ref os and file = ref None in
  Arg.parse
    [ ("-o", Set_string os, "The operating system to generate for") ]
    (fun x -> file := Some x)
    "";
  let { file_contents; _ } =
    match !file with
    | Some x -> OpamParser.FullPos.file x
    | None -> OpamParser.FullPos.channel stdin "=stdin"
  in
  let fields = List.fold_left (to_json ~os:!os) [] file_contents in
  let json : J.t =
    `Assoc
      (("name", `String "illuaminate") :: List.rev fields
      @ [ ( "esy",
            `Assoc
              [ ("build", `String "dune build -p #{self.name}");
                ("release", `Assoc [ ("includePackages", `List [ `String "root" ]) ]);
                ("buildEnv", `Assoc [ ("PATH", `String "#{self.root / '_build_tools' : $PATH}") ])
              ] );
          ( "resolutions",
            `Assoc
              [ ( "@opam/omnomnom",
                  `String ("git://github.com/SquidDev/omnomnom:omnomnom.opam#" ^ Versions.omnomnom)
                );
                ( "@opam/lsp-fiber",
                  `String
                    ("git://github.com/SquidDev/ocaml-lsp-subtree:lsp-fiber.opam#" ^ Versions.lsp)
                )
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
