open Omnomnom.Tests

let () =
  Omnomnom.run
    ~reporter:Omnomnom.Ingredients.(compose_reporters console_reporter OmnomnomJUnit.reporter)
  @@ group "illuaminate"
       [ Parser.tests;
         Lexer.tests;
         Config.tests;
         Lint.tests;
         Pattern.tests;
         Reprint.tests;
         Doc_parser.tests
       ]
