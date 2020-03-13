open OpamParserTypes
module J = Yojson.Safe

module Versions = struct
  let omnomnom = "a0620d42b2ac94fcc4617e87709d6bda9188362c"

  let lsp = "f3e1f4c543eef14a3cc42bc5efbb41c9137dc3a2"
end

let rec json_of_value : value -> J.t = function
  | String (_, x) | Ident (_, x) -> `String x
  | Bool (_, x) -> `Bool x
  | Int (_, x) -> `Int x
  | List (_, xs) -> `List (List.map json_of_value xs)
  | (Pfxop _ | Option _ | Relop _ | Prefix_relop _ | Group _ | Logop _ | Env_binding _) as x ->
      `String (OpamPrinter.value x)

type dep_kind =
  | Main
  | Dev
  | Skip

let dep_of_value ~os : value -> dep_kind * string * string = function
  | String (_, p) -> (Main, p, "*")
  | Option (_, String (_, p), constraints) ->
      let rec flat xs = function
        | Logop (_, `And, l, r) -> flat (flat xs l) r
        | x -> x :: xs
      in
      let rec build (kind, version) = function
        | [] -> (kind, version)
        | Ident (_, ("with-test" | "with-doc" | "build" | "dev")) :: xs -> build (Dev, version) xs
        | Prefix_relop (_, op, String (_, v)) :: xs -> (
          match version with
          | Some _ -> Printf.sprintf "Multiple versions for '%s'" p |> failwith
          | None -> build (kind, Some (OpamPrinter.relop op ^ " " ^ v)) xs )
        | Relop (_, `Neq, Ident (_, "os-family"), String (_, this_os)) :: xs ->
            if this_os <> os then build (kind, version) xs else (Skip, version)
        | c :: _ ->
            Printf.sprintf "Unknown constraint '%s' for '%s'" (OpamPrinter.value c) p |> failwith
      in
      let kind, version = List.fold_left flat [] constraints |> build (Main, None) in
      (kind, p, Option.value ~default:"*" version)
  | p -> Printf.sprintf "Unknown package '%s'" (OpamPrinter.value p) |> failwith

let to_json ~os fields : opamfile_item -> (string * J.t) list =
  let add_field k v = (k, json_of_value v) :: fields in
  function
  | Variable (_, (("version" | "license" | "homepage") as k), v) -> add_field k v
  | Variable (_, "synopsis", x) -> add_field "description" x
  | Variable (_, "depends", List (_, depends)) ->
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
      let main, dev = List.fold_left add ([], []) depends in
      ("devDependencies", `Assoc (List.rev dev))
      :: ("dependencies", `Assoc (List.rev main))
      :: fields
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
    | Some x -> OpamParser.file x
    | None -> OpamParser.channel stdin "=stdin"
  in
  let fields = List.fold_left (to_json ~os:!os) [] file_contents in
  let json : J.t =
    `Assoc
      ( (("name", `String "illuaminate") :: List.rev fields)
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
                ("@opam/lsp", `String ("git://github.com/ocaml/ocaml-lsp:lsp.opam#" ^ Versions.lsp))
              ] );
          ( "scripts",
            `Assoc
              [ ("test", `String "dune build @runtest -f");
                ("format", `String "dune build @fmt --auto-promote")
              ] )
        ] )
  in
  J.pretty_to_channel ~std:true stdout json;
  print_newline ()
