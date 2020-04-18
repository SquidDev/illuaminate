%token FUNCTION NIL TRUE FALSE

%token COMMA ","
%token COLON ":"
%token DOT "."
%token DOTS "..."
%token EQUALS "="
%token SEMICOLON ";"
%token PIPE "|"
%token ASKING "?"

%token OPAREN "(" CPAREN ")"
%token OBRACE "{" CBRACE "}"
%token OSQUARE "[" CSQUARE "]"

%token <string> IDENT STRING
%token <int> INT
%token <float> NUMBER

%token EOF

%start <Type_syntax.Unresolved.t> main
%start <bool * Type_syntax.Unresolved.t> main_vararg

%type <Type_syntax.Unresolved.t> simple_type ty
%type <Type_syntax.Unresolved.arg> arg

%{ open Type_syntax.Unresolved %}

%%

let main := ~ = ty ; EOF ; <>

let main_vararg := ~ = var_ty ; EOF ; <>

let name := ~ = IDENT ; <>

let arg_name :=
  | ~ = name ; { name }
  | "..." ;    { "..." }

let arg :=
  | ~ = arg_name ; ":" ; ~ = ty ; { { name = Some arg_name; ty; opt = false } }
  | (many, ty) = var_ty ;         { { name = if many then Some "..." else None; ty; opt = false } }

let args_rest :=
  | { [] }
  | "[" ; "," ; ~ = arg ; ~ = args_rest ; "]" ; { { arg with opt = true } :: args_rest }
  | "," ;  ~ = arg ; ~ = args_rest ;            { { arg with opt = false } :: args_rest }

let args :=
  | { [] }
  | "[" ; ~ = arg ; ~ = args_rest ; "]" ; { { arg with opt = true } :: args_rest }
  | ~ = arg ; ~ = args_rest ;             { { arg with opt = false } :: args_rest }

let return :=
  |                                       { ([], None) }
  | ":" ; ty = simple_type ;              { ([ty], None) }
  | ":" ; ty = simple_type ; "..." ;      { ([], Some ty) }

let var :=
  | ~ = name ;                            <>
  | tbl = var ; "." ; field = name ;      { tbl ^ "." ^ field }

let simple_type :=
  | ~ = var ;                         { Named (Reference var, var) }
  | FUNCTION ;                        { Named (Reference "function", "function") }
  | "(" ; ~ =  ty ; ")" ;             <>
  | NIL ;                             { NilTy }
  | TRUE ;                            { BoolTy true }
  | FALSE ;                           { BoolTy false }
  | ~ = STRING ;                      <StringTy>
  | ~ = NUMBER ;                      <NumberTy>
  | ~ = INT ;                         <IntTy>
  | "{" ; ~ = table_body ; "}";       <Table>

let function_type :=
  | ~ = simple_type ;                 <>
  | FUNCTION ; "(" ; ~ = args ; ")" ; ~ = return ;
  { Function { args; return; } }

let ty :=
  | x = separated_nonempty_list("|", function_type) ;
    { match x with
      | [] -> assert false
      | [x] -> x
      | xs -> Union xs }

let var_ty :=
  | ~ = ty ;                           { (false, ty) }
  | ty = simple_type ; "..." ;         { (true, ty) }

let table_sep := ";" | ","

let table_body :=
  | { [] }
  | x = table_entry ; { [x] }
  | x = table_entry ; table_sep ; xs = table_body ; { x :: xs }

let optional :=
  |                                    { false }
  | "?" ;                              { true }

let table_entry :=
  | (many, ty) = var_ty ;
    { if many then Many ty else Item ty }
  | key = IDENT ; ~ = optional ; "=" ; value = ty ;
    { Field { key; optional; value } }
  | "[" ; key = ty ; "]" ; ~ = optional ;"=" ; value = ty ;
    { Hash { key; optional; value } }
