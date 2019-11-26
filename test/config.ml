open Omnomnom.Tests
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
  buf.lex_curr_p <- { buf.lex_curr_p with Lexing.pos_fname = name };
  match Term.to_parser spec |> Parser.fields |> Parser.parse_buf buf with
  | Ok k -> show_config (proj k)
  | Error ({ row; col }, e) -> Printf.sprintf "%s:%d:%d: %s" name row col e

let to_string spec ~name:_ = Format.asprintf "%a" Term.write_default spec

let tests =
  group "The config system"
    [ OmnomnomGolden.of_directory (process spec Fun.id) ~group:"Basic terms"
        ~directory:"data/config" ~extension:".sexp" ();
      OmnomnomGolden.of_directory
        (process (Schema.singleton spec_key |> Schema.to_term) (Schema.get spec_key))
        ~group:"Categories" ~directory:"data/config-cats" ~extension:".sexp" ();
      group "Writes defaults"
        [ OmnomnomGolden.of_output (to_string spec) ~directory:"data/config"
            ~output_name:"default.out";
          OmnomnomGolden.of_output
            (Schema.singleton spec_key |> Schema.to_term |> to_string)
            ~directory:"data/config-cats" ~output_name:"default.out"
        ]
    ]
