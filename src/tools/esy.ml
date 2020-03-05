open OpamParserTypes
module J = Yojson.Safe

module Versions = struct
  let omnomnom = "6b6d503d45f41a8a4f6af042e19d9278aa403c25"
end

let rec json_of_value : value -> J.t = function
  | String (_, x) | Ident (_, x) -> `String x
  | Bool (_, x) -> `Bool x
  | Int (_, x) -> `Int x
  | List (_, xs) -> `List (List.map json_of_value xs)
  | (Pfxop _ | Option _ | Relop _ | Prefix_relop _ | Group _ | Logop _ | Env_binding _) as x ->
      `String (OpamPrinter.value x)

let dep_of_value : value -> bool * string * string = function
  | String (_, p) -> (false, p, "*")
  | Option (_, String (_, p), [ Ident (_, ("with-test" | "with-doc" | "build")) ]) -> (true, p, "*")
  | Option (_, String (_, p), [ Prefix_relop (_, op, String (_, v)) ]) ->
      (true, p, OpamPrinter.relop op ^ " " ^ v)
  | p -> Printf.sprintf "Unknown package '%s'" (OpamPrinter.value p) |> failwith

type result =
  { fields : (string * J.t) list;
    dependencies : (string * J.t) list;
    devDependencies : (string * J.t) list
  }

let to_json result : opamfile_item -> result =
  let add_field k v = { result with fields = (k, json_of_value v) :: result.fields } in
  function
  | Variable (_, (("version" | "license" | "homepage") as k), v) -> add_field k v
  | Variable (_, "synopsis", x) -> add_field "description" x
  | Variable (_, "depends", List (_, depends)) ->
      let add r v =
        let is_dev, name, version = dep_of_value v in
        let dep = ("@opam/" ^ name, `String version) in
        if is_dev then { r with devDependencies = dep :: r.devDependencies }
        else { r with dependencies = dep :: r.dependencies }
      in
      List.fold_left add result depends
  | _ -> result

let () =
  let { file_contents; _ } =
    match Sys.argv with
    | [| _; x |] -> OpamParser.file x
    | [| _ |] -> OpamParser.channel stdin "=stdin"
    | _ ->
        Printf.eprintf "%s: [FILE]\n%!" Sys.executable_name;
        exit 1
  in
  let { fields; dependencies; devDependencies } =
    List.fold_left to_json { fields = []; dependencies = []; devDependencies = [] } file_contents
  in
  let json : J.t =
    `Assoc
      ( (("name", `String "illuaminate") :: List.rev fields)
      @ [ ( "esy",
            `Assoc
              [ ("build", `String "dune build -p #{self.name}");
                ("release", `Assoc [ ("includePackages", `List [ `String "root" ]) ])
              ] );
          ("dependencies", `Assoc (("ocaml", `String ">= 4.08") :: List.rev dependencies));
          ("devDependencies", `Assoc (List.rev devDependencies));
          ( "resolutions",
            `Assoc
              [ ( "@opam/omnomnom",
                  `String ("git://github.com/SquidDev/omnomnom:omnomnom.opam#" ^ Versions.omnomnom)
                )
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
