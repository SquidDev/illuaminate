open Omnomnom.Tests
open IlluaminateCore
open IlluaminateConfig

type config =
  { name : string;
    age : int
  }
[@@deriving show]

let spec =
  let open Term in
  let open Converter in
  group ~name:"pet" ~comment:"A description of your pet"
    (let+ name = field ~name:"name" ~comment:"Your pet's name" ~default:"Keith" string
     and+ age = field ~name:"age" ~comment:"Your pet's age" ~default:3 int in
     { name; age })

let cat = Category.create ~name:"companions" ~comment:"Your companions in life." ()
let spec_key = Category.add spec cat

let process spec proj ~name contents =
  let buf = Lexing.from_string contents in
  match spec |> Parser.fields |> Parser.parse_buf (Span.Filename.mk name) buf with
  | Ok k -> show_config (proj k)
  | Error (pos, e) -> Format.asprintf "%a: %s" Span.pp pos e

let to_string f spec ~name:_ = Format.asprintf "%a" f spec

let tests =
  group "The config system"
    [ OmnomnomGolden.of_directory
        (process (Term.to_parser spec) Fun.id)
        ~group:"Basic terms" ~directory:"data/config" ~extension:".sexp" ();
      OmnomnomGolden.of_directory
        (process (Schema.singleton spec_key |> Schema.to_parser) (Schema.get spec_key))
        ~group:"Categories" ~directory:"data/config-cats" ~extension:".sexp" ();
      group "Writes defaults"
        [ OmnomnomGolden.of_output
            (to_string Term.write_default spec)
            ~directory:"data/config" ~output_name:"default.out";
          OmnomnomGolden.of_output
            (Schema.singleton spec_key |> to_string Schema.write_default)
            ~directory:"data/config-cats" ~output_name:"default.out"
        ]
    ]
