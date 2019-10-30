type token = Token.t Node.t [@@deriving show]

module SepList1 = struct
  type 'a t =
    | Mono of 'a
    | Cons1 of 'a * token * 'a t
  [@@deriving show]

  let rec iter fx ?(tok = fun _ -> ()) = function
    | Mono x -> fx x
    | Cons1 (x, t, xs) -> fx x; tok t; iter fx ~tok xs

  let rec map fx ?(tok = Fun.id) = function
    | Mono x -> Mono (fx x)
    | Cons1 (x, t, xs) -> Cons1 (fx x, tok t, map fx ~tok xs)

  let rec map' f = function
    | Mono x -> [ f x ]
    | Cons1 (x, _, xs) -> f x :: map' f xs

  let rec map_with' f s = function
    | Mono x ->
        let s, x = f s x in
        (s, [ x ])
    | Cons1 (x, _, xs) ->
        let s, x = f s x in
        let s, xs = map_with' f s xs in
        (s, x :: xs)

  let rec filter_map' f = function
    | Mono x -> (
      match f x with
      | None -> []
      | Some x -> [ x ] )
    | Cons1 (x, _, xs) -> (
      match f x with
      | None -> filter_map' f xs
      | Some x -> x :: filter_map' f xs )

  let rec fold_left f s = function
    | Mono x -> f s x
    | Cons1 (x, _, xs) -> fold_left f (f s x) xs

  let iteri f xs = fold_left (fun i x -> f i x; i + 1) 0 xs |> ignore

  let first =
    let get = function
      | Mono x -> x
      | Cons1 (x, _, _) -> x
    and over f = function
      | Mono x -> Mono (f x)
      | Cons1 (x, s, xs) -> Cons1 (f x, s, xs)
    in
    { Lens.get; Lens.over }

  let last =
    let rec get = function
      | Mono x -> x
      | Cons1 (_, _, xs) -> get xs
    and over f = function
      | Mono x -> Mono (f x)
      | Cons1 (x, s, xs) -> Cons1 (x, s, over f xs)
    in
    { Lens.get; Lens.over }

  let span f = function
    | Mono x -> f x
    | Cons1 (x, _, xs) -> Span.of_span2 (f x) (last.get xs |> f)

  let length x = fold_left (fun x _ -> x + 1) 0 x
end

module SepList0 = struct
  type 'a t = 'a SepList1.t option [@@deriving show]

  let map' f = function
    | None -> []
    | Some x -> SepList1.map' f x

  let map_with' f s = function
    | None -> (s, [])
    | Some x -> SepList1.map_with' f s x

  let fold_left f s = function
    | None -> s
    | Some x -> SepList1.fold_left f s x

  let map f ?tok = Option.map (SepList1.map f ?tok)

  let iter f ?tok = Option.iter (SepList1.iter f ?tok)

  let last x = Option.map SepList1.last.get x

  let length x = fold_left (fun x _ -> x + 1) 0 x
end

module UnOp = struct
  type t =
    | OpNeg
    | OpLen
    | OpNot
    | OpBNot

  let to_token = function
    | OpNeg -> Token.Sub
    | OpLen -> Token.Len
    | OpNot -> Token.Not
    | _ -> assert false

  let of_token = function
    | Token.Sub -> OpNeg
    | Token.Len -> OpLen
    | Token.Not -> OpNot
    | t -> failwith (Format.asprintf "Cannot convert token '%a' to unop." Token.pp t)

  let token = { Lens.get = to_token; Lens.over = (fun f x -> to_token x |> f |> of_token) }

  let show = function
    | OpNeg -> "-"
    | OpLen -> "#"
    | OpNot -> "not"
    | OpBNot -> "~"

  let pp f x = Format.pp_print_string f (show x)

  let precedence (_ : t) = 1
end

module BinOp = struct
  type t =
    | OpAdd
    | OpSub
    | OpMul
    | OpDiv
    | OpMod
    | OpPow
    | OpConcat
    | OpEq
    | OpNe
    | OpLt
    | OpLe
    | OpGt
    | OpGe
    | OpAnd
    | OpOr

  let show = function
    | OpAdd -> "+"
    | OpSub -> "-"
    | OpMul -> "*"
    | OpDiv -> "/"
    | OpMod -> "%"
    | OpPow -> "^"
    | OpConcat -> ".."
    | OpEq -> "=="
    | OpNe -> "~="
    | OpLt -> "<"
    | OpLe -> "<="
    | OpGt -> ">"
    | OpGe -> ">="
    | OpAnd -> "and"
    | OpOr -> "or"

  let pp f x = Format.pp_print_string f (show x)

  let precedence = function
    | OpPow -> 0
    | OpMul | OpDiv | OpMod -> 2
    | OpAdd | OpSub -> 3
    | OpConcat -> 4
    | OpEq | OpNe | OpLt | OpLe | OpGt | OpGe -> 5
    | OpAnd -> 6
    | OpOr -> 7

  let left_precedence o =
    match o with
    | OpPow | OpConcat -> precedence o
    | _ -> precedence o + 1

  let right_precedence o =
    match o with
    | OpPow | OpConcat -> precedence o + 1
    | _ -> precedence o
end

(** [do print("Hello") end] *)
type do_stmt =
  { do_do : token;  (** [do] *)
    do_body : block;  (** [print("Hello")] The do block's body. *)
    do_end : token  (** [end] *)
  }

(** [x, y.z = 1, 2] *)
and assign_stmt =
  { assign_vars : name SepList1.t;  (** [x, y.z] (The variables/tables to assign to) *)
    assign_eq : token;  (** [=] *)
    assign_vals : expr SepList1.t  (** [1, 2] (The expressions to assign from) *)
  }

(** [while true do print("Hello") end] *)
and while_stmt =
  { while_while : token;  (** [while] *)
    while_test : expr;  (** [true] (The test to determine if the loop continues) *)
    while_do : token;  (** [do] *)
    while_body : block;  (** [print("Hello")] (The loop's body) *)
    while_end : token  (** [end] *)
  }

(** [repeat print("Hello") until false] *)
and repeat_stmt =
  { repeat_repeat : token;  (** [repeat] *)
    repeat_body : block;  (** [print("Hello")] (The loop's body) *)
    repeat_until : token;  (** [until] *)
    repeat_test : expr  (** [false] (The test to determine if the loop stops) *)
  }

(** [for i = 1, 10, 1 do print(i) end] *)
and for_num_stmt =
  { forn_for : token;  (** [for] *)
    forn_var : var;  (** [i] (The loop counter) *)
    forn_eq : token;  (** [=] *)
    forn_start : expr;  (** [1] (The start value) *)
    forn_comma : token;  (** [,] *)
    forn_limit : expr;  (** [10] (The end value) *)
    forn_step : (token * expr) option;
        (** [, 1] (The comma separator, and value to change the counter by each iteration) *)
    forn_do : token;  (** [do] *)
    forn_body : block;  (** [print(i)] (The loop body) *)
    forn_end : token  (** [end] *)
  }

(** [for k, v in next, tbl do print(k, v) end] *)
and for_in_stmt =
  { forp_for : token;  (** [for] *)
    forp_vars : var SepList1.t;  (** [k, v] (The variables to bind this iteration to. *)
    forp_in : token;  (** [in] *)
    forp_iter : expr SepList1.t;
        (** [next, tbl] (One or more expressions providing the iteration function and state. *)
    forp_do : token;  (** [do] *)
    forp_body : block;  (** [print(k, v)] (The loop body) *)
    forp_end : token  (** [end] *)
  }

(** [local a, b = 1, 2] *)
and local_stmt =
  { local_local : token;  (** [local] *)
    local_vars : var SepList1.t;  (** [a, b] (The variables to declare)*)
    local_vals : (token * expr SepList1.t) option
        (** [= 1, 2] (The equal sign, plus the expressions to assign) *)
  }

(** [local function f(x, ...) return x end] *)
and local_function_stmt =
  { localf_local : token;  (** [local] *)
    localf_function : token;  (** [function] *)
    localf_var : var;  (** [f] (The function's name. *)
    localf_args : args;  (** [(x, ...)] (The function's argument) *)
    localf_body : block;  (** [return x] (The function's body) *)
    localf_end : token  (** [end] *)
  }

(** [function f(x, ...) return x end] *)
and function_stmt =
  { assignf_function : token;  (** [function] *)
    assignf_name : function_name;  (** [f] (The function's name) *)
    assignf_args : args;  (** [(x, ...)] (The function's arguments) *)
    assignf_body : block;  (** [return x] (The function's body) *)
    assignf_end : token  (** [end] *)
  }

(** Like {!name}, but without the more general table case. *)
and function_name =
  | FVar of var  (** [x] *)
  | FDot of
      { tbl : function_name;
        dot : token;
        field : string Node.t
      }  (** [x.y] *)
  | FSelf of
      { tbl : function_name;
        colon : token;
        meth : string Node.t
      }  (** [x:y] *)

(** [return x, y] *)
and return_stmt =
  { return_return : token;  (** [return] *)
    return_vals : expr SepList0.t  (** [x, y] (The expression to return. *)
  }

(** [if true then print("Hello") elseif false then print("Hello") else print("Hello") end] *)
and if_stmt =
  { if_if : if_clause;  (** [if true then print("Hello")] (The primary test) *)
    if_elseif : if_clause list;  (** [elseif false then print("Hello")] (Auxiliary clauses) *)
    if_else : (token * block) option;  (** [else print("Hello")] (Optional else clause) *)
    if_end : token  (** [end] *)
  }

(** [if true then print("Hello")] - A single [if]/[elseif] clause in an if statement *)
and if_clause =
  { clause_if : token;  (** [if]/[elseif] *)
    clause_test : expr;  (** [true] (The clause's test) *)
    clause_then : token;  (** [then] *)
    clause_body : block  (** [print("Hello")] (The clause's body) *)
  }

and stmt =
  | Do of do_stmt
  | Assign of assign_stmt
  | While of while_stmt
  | Repeat of repeat_stmt
  | ForNum of for_num_stmt
  | ForIn of for_in_stmt
  | Local of local_stmt
  | LocalFunction of local_function_stmt
  | AssignFunction of function_stmt
  | Return of return_stmt
  | If of if_stmt
  | Break of token
  | SCall of call
  | Semicolon of token

and block = stmt list

(** A raw variable or identifier to bind to. *)
and var = Var of string Node.t  (** [x] *) [@@unboxed]

and name =
  | NVar of var  (** [x] *)
  | NDot of
      { tbl : expr;  (** [t] (The table to index) *)
        dot : token;  (** [.] *)
        key : string Node.t  (** [x] (The table key) *)
      }  (** [t.x] - Index a table with an identifier key. *)
  | NLookup of
      { tbl : expr;  (** [t] (The table to index) *)
        open_k : token;  (** [\[] *)
        key : expr;  (** [x] (The table key) *)
        close_k : token  (** [\]] *)
      }  (** [\[ t\[x\] \]] - Index a table with an expression key. *)

(** A literal value. *)
and 'a literal =
  { lit_value : 'a;  (** The underlying value. *)
    lit_node : string Node.t
        (** The node that this value was parsed from, with its string representation. *)
  }

(** [function(x, ...) return x end] *)
and fun_expr =
  { fun_function : token;  (** [function] *)
    fun_args : args;  (** [(x, ...)] (The function's arguments) *)
    fun_body : block;  (** [return x] (The function's body) *)
    fun_end : token  (** [end] *)
  }

and table =
  { table_open : token;  (** [{] *)
    table_body : (table_item * token option) list;
        (** [x,] (Node, and optional separator). Note, this isn't perfect - allows you to represent
            tables with no separators, but seems the best compromise. *)
    table_close : token  (** [}] *)
  }

and table_item =
  | Array of expr
  | RawPair of
      { ident : string Node.t;  (** [x] *)
        eq : token;  (** [=] *)
        value : expr  (** 1 *)
      }  (** x = 1 *)
  | ExprPair of
      { open_k : token;  (** [\[] *)
        key : expr;  (** [x] *)
        close_k : token;  (** [\]] *)
        eq : token;  (** [=] *)
        value : expr  (** 1 *)
      }  (** [\[ \[x\] = 1 \]] *)

(** [#x] *)
and unop_expr =
  { unop_op : UnOp.t Node.t;  (** [#] *)
    unop_rhs : expr  (** [x] *)
  }

(** [x + y] *)
and binop_expr =
  { binop_lhs : expr;  (** [x] *)
    binop_op : BinOp.t Node.t;  (** [+] *)
    binop_rhs : expr  (** [y] *)
  }

(** [(x)] *)
and paren_expr =
  { paren_open : token;  (** [(] *)
    paren_expr : expr;  (** [x] *)
    paren_close : token  (** [)] *)
  }

(** A raw expression. *)
and expr =
  | Ref of name
  | ECall of call
  | Dots of token
  | Nil of token
  | True of token
  | False of token
  | Number of float literal
  | Int of int literal
  | String of string literal
  | Fun of fun_expr
  | Table of table
  | UnOp of unop_expr
  | BinOp of binop_expr
  | Parens of paren_expr

(** A function or method application. *)
and call =
  | Call of
      { fn : expr;
        args : call_args
      }  (** [f(x)] *)
  | Invoke of
      { fn : expr;
        colon : token;
        meth : string Node.t;
        args : call_args
      }  (** [o:f(x)] *)

(** The arguments to a function call. *)
and call_args =
  | CallArgs of
      { open_a : token;
        args : expr SepList0.t;
        close_a : token
      }
  | CallTable of table
  | CallString of string literal

(** The arguments to a function definition. *)
and args =
  { args_open : token;
    args_args : arg SepList0.t;
    args_close : token
  }

and arg =
  | NamedArg of var
  | DotArg of token

and program =
  { program : block;
    eof : token
  }
[@@deriving show, illuaminateDeriving_traverse { prefix = "_" }, illuaminateDeriving_lens]

class iter =
  object (self)
    inherit _iter

    method string (_ : string) = ()

    method int (_ : int) = ()

    method float (_ : float) = ()

    method unit (_ : unit) = ()

    method bool (_ : bool) = ()

    method unop (_ : UnOp.t) = ()

    method binop (_ : BinOp.t) = ()

    method token (tok : token) = self#node (fun _ -> ()) tok

    method node : 'a. ('a -> unit) -> 'a Node.t -> unit = fun f x -> Node.contents.get x |> f

    method option : 'a. ('a -> unit) -> 'a option -> unit = Option.iter

    method list : 'a. ('a -> unit) -> 'a list -> unit = List.iter

    method seplist1 : 'a. ('a -> unit) -> 'a SepList1.t -> unit =
      fun f -> SepList1.iter f ~tok:self#token

    method seplist0 : 'a. ('a -> unit) -> 'a SepList0.t -> unit =
      fun f -> SepList0.iter f ~tok:self#token
  end

class map =
  object (self)
    inherit _map

    method string (x : string) = x

    method int (x : int) = x

    method float (x : float) = x

    method unit (x : unit) = x

    method bool (x : bool) = x

    method unop (x : UnOp.t) = x

    method binop (x : BinOp.t) = x

    method token (tok : token) = self#node Fun.id tok

    method node : 'a. ('a -> 'a) -> 'a Node.t -> 'a Node.t = Node.contents.over

    method option : 'a. ('a -> 'a) -> 'a option -> 'a option = Option.map

    method list : 'a. ('a -> 'a) -> 'a list -> 'a list = List.map

    method seplist1 : 'a. ('a -> 'a) -> 'a SepList1.t -> 'a SepList1.t =
      fun f -> SepList1.map f ~tok:self#token

    method seplist0 : 'a. ('a -> 'a) -> 'a SepList0.t -> 'a SepList0.t =
      fun f -> SepList0.map f ~tok:self#token
  end

(** Get the first token within a term. *)
module First = struct
  open Lens

  let do_stmt = Do_stmt.do_do

  let while_stmt = While_stmt.while_while

  let repeat_stmt = Repeat_stmt.repeat_repeat

  let for_num_stmt = For_num_stmt.forn_for

  let for_in_stmt = For_in_stmt.forp_for

  let local_stmt = Local_stmt.local_local

  let local_function_stmt = Local_function_stmt.localf_local

  let function_stmt = Function_stmt.assignf_function

  let return_stmt = Return_stmt.return_return

  let if_clause = If_clause.clause_if

  let if_stmt = If_stmt.if_if -| if_clause

  let ident =
    let get x = Token.Ident x
    and over f x =
      match f (Token.Ident x) with
      | Token.Ident x -> x
      | t -> failwith (Format.asprintf "Cannot convert token '%a' to identifier." Token.pp t)
    in
    Node.lens_embed { get; over }

  let var = { get = (fun (Var x) -> x); over = (fun f (Var x) -> Var (f x)) } -| ident

  let function_name =
    let rec get = function
      | FVar x -> var.get x
      | FDot { tbl; _ } -> get tbl
      | FSelf { tbl; _ } -> get tbl
    and over f = function
      | FVar x -> FVar (var.over f x)
      | FDot ({ tbl; _ } as rest) -> FDot { rest with tbl = over f tbl }
      | FSelf ({ tbl; _ } as rest) -> FSelf { rest with tbl = over f tbl }
    in
    { get; over }

  let args = Args.args_open

  let arg =
    let get = function
      | NamedArg x -> var.get x
      | DotArg x -> x
    and over f = function
      | NamedArg x -> NamedArg (var.over f x)
      | DotArg x -> DotArg (f x)
    in
    { get; over }

  let fun_expr = Fun_expr.fun_function

  let table = Table.table_open

  let unop_expr = Unop_expr.unop_op -| Node.lens_embed UnOp.token

  let paren_expr = Paren_expr.paren_open

  let literal_embed (get : 'a -> string -> Token.t) (set : Token.t -> 'a * string) =
    let get { lit_node; lit_value } = (Node.contents %= fun x -> get lit_value x) @@ lit_node
    and over f { lit_node; lit_value } =
      let res = (Node.contents %= fun x -> get lit_value x) @@ lit_node |> f in
      let lit_value, contents = res ^. Node.contents |> set in
      { lit_node = Node.with_contents contents res; lit_value }
    in
    { get; over }

  let lit_string =
    let get v t = Token.String (v, t)
    and set = function
      | Token.String (v, t) -> (v, t)
      | t -> failwith (Format.asprintf "Cannot convert token '%a' to string." Token.pp t)
    in
    literal_embed get set

  let lit_number =
    let get v t = Token.Number (v, t)
    and set = function
      | Token.Number (v, t) -> (v, t)
      | t -> failwith (Format.asprintf "Cannot convert token '%a' to number." Token.pp t)
    in
    literal_embed get set

  let lit_int =
    let get v t = Token.Int (v, t)
    and set = function
      | Token.Int (v, t) -> (v, t)
      | t -> failwith (Format.asprintf "Cannot convert token '%a' to int." Token.pp t)
    in
    literal_embed get set

  let rec stmt =
    let get = function
      | Do x -> do_stmt.get x
      | Assign x -> assign_stmt.get x
      | While x -> while_stmt.get x
      | Repeat x -> repeat_stmt.get x
      | ForNum x -> for_num_stmt.get x
      | ForIn x -> for_in_stmt.get x
      | Local x -> local_stmt.get x
      | LocalFunction x -> local_function_stmt.get x
      | AssignFunction x -> function_stmt.get x
      | Return x -> return_stmt.get x
      | If x -> if_stmt.get x
      | Break x -> x
      | SCall x -> call.get x
      | Semicolon x -> x
    and over f = function
      | Do x -> Do (do_stmt.over f x)
      | Assign x -> Assign (assign_stmt.over f x)
      | While x -> While (while_stmt.over f x)
      | Repeat x -> Repeat (repeat_stmt.over f x)
      | ForNum x -> ForNum (for_num_stmt.over f x)
      | ForIn x -> ForIn (for_in_stmt.over f x)
      | Local x -> Local (local_stmt.over f x)
      | LocalFunction x -> LocalFunction (local_function_stmt.over f x)
      | AssignFunction x -> AssignFunction (function_stmt.over f x)
      | Return x -> Return (return_stmt.over f x)
      | If x -> If (if_stmt.over f x)
      | Break x -> Break x
      | SCall x -> SCall (call.over f x)
      | Semicolon x -> Semicolon x
    in
    { get; over }

  and assign_stmt =
    let get x = Assign_stmt.assign_vars.get x |> SepList1.first.get |> name.get
    and over f = Assign_stmt.assign_vars.over (SepList1.first.over (name.over f)) in
    { get; over }

  and name =
    let get = function
      | NVar x -> var.get x
      | NDot { tbl; _ } -> expr.get tbl
      | NLookup { tbl; _ } -> expr.get tbl
    and over f = function
      | NVar x -> NVar (var.over f x)
      | NDot ({ tbl; _ } as rest) -> NDot { rest with tbl = expr.over f tbl }
      | NLookup ({ tbl; _ } as rest) -> NLookup { rest with tbl = expr.over f tbl }
    in
    { get; over }

  and table_item =
    let get = function
      | Array x -> expr.get x
      | RawPair { ident = id; _ } -> ident.get id
      | ExprPair { open_k; _ } -> open_k
    and over f = function
      | Array x -> Array (expr.over f x)
      | RawPair ({ ident = id; _ } as rest) -> RawPair { rest with ident = ident.over f id }
      | ExprPair ({ open_k; _ } as rest) -> ExprPair { rest with open_k = f open_k }
    in
    { get; over }

  and binop_expr =
    let get x = Binop_expr.binop_lhs.get x |> expr.get
    and over f = Binop_expr.binop_lhs.over (expr.over f) in
    { get; over }

  and expr =
    let get = function
      | Ref x -> name.get x
      | ECall x -> call.get x
      | Dots x | Nil x | True x | False x -> x
      | Number x -> lit_number.get x
      | Int x -> lit_int.get x
      | String x -> lit_string.get x
      | Fun x -> fun_expr.get x
      | Table x -> table.get x
      | UnOp x -> unop_expr.get x
      | BinOp x -> binop_expr.get x
      | Parens x -> paren_expr.get x
    and over f = function
      | Ref x -> Ref (name.over f x)
      | ECall x -> ECall (call.over f x)
      | Dots x -> Dots (f x)
      | Nil x -> Nil (f x)
      | True x -> True (f x)
      | False x -> False (f x)
      | Number x -> Number (lit_number.over f x)
      | Int x -> Int (lit_int.over f x)
      | String x -> String (lit_string.over f x)
      | Fun x -> Fun (fun_expr.over f x)
      | Table x -> Table (table.over f x)
      | UnOp x -> UnOp (unop_expr.over f x)
      | BinOp x -> BinOp (binop_expr.over f x)
      | Parens x -> Parens (paren_expr.over f x)
    in
    { get; over }

  and call =
    let get = function
      | Call { fn; _ } -> expr.get fn
      | Invoke { fn; _ } -> expr.get fn
    and over f = function
      | Call ({ fn; _ } as rest) -> Call { rest with fn = expr.over f fn }
      | Invoke ({ fn; _ } as rest) -> Invoke { rest with fn = expr.over f fn }
    in
    { get; over }

  and call_args = function
    | CallArgs { open_a; _ } -> open_a
    | CallTable x -> table.get x
    | CallString { lit_node = h; lit_value } ->
        Node.with_contents (Token.String (lit_value, h ^. Node.contents)) h

  let program =
    let get { program; eof } =
      match program with
      | [] -> eof
      | x :: _ -> stmt.get x
    and over f { program; eof } =
      match program with
      | [] -> { program; eof = f eof }
      | x :: xs -> { program = stmt.over f x :: xs; eof }
    in
    { get; over }
end

(** Get the last token within a term. *)
module Last = struct
  open Lens

  let do_stmt = Do_stmt.do_end

  let while_stmt = While_stmt.while_end

  let for_num_stmt = For_num_stmt.forn_end

  let for_in_stmt = For_in_stmt.forp_end

  let function_stmt = Function_stmt.assignf_end

  let local_function_stmt = Local_function_stmt.localf_end

  let if_stmt = If_stmt.if_end

  let var = First.var

  let name =
    let get = function
      | NVar x -> var.get x
      | NDot { key; _ } -> First.ident.get key
      | NLookup { close_k; _ } -> close_k
    and over f = function
      | NVar x -> NVar (var.over f x)
      | NDot ({ key; _ } as rest) -> NDot { rest with key = First.ident.over f key }
      | NLookup ({ close_k; _ } as rest) -> NLookup { rest with close_k = f close_k }
    in
    { get; over }

  let fun_expr = Fun_expr.fun_end

  let table = Table.table_close

  let call_args =
    let get = function
      | CallArgs { close_a; _ } -> close_a
      | CallTable x -> table.get x
      | CallString x -> First.lit_string.get x
    and over f = function
      | CallArgs ({ close_a; _ } as rest) -> CallArgs { rest with close_a = f close_a }
      | CallTable x -> CallTable (table.over f x)
      | CallString x -> CallString (First.lit_string.over f x)
    in
    { get; over }

  let call =
    let get = function
      | Call { args; _ } -> call_args.get args
      | Invoke { args; _ } -> call_args.get args
    and over f = function
      | Call ({ args; _ } as rest) -> Call { rest with args = call_args.over f args }
      | Invoke ({ args; _ } as rest) -> Invoke { rest with args = call_args.over f args }
    in
    { get; over }

  let args = Args.args_close

  let rec unop_expr =
    let get { unop_rhs; _ } = expr.get unop_rhs
    and over f ({ unop_rhs; _ } as rest) = { rest with unop_rhs = expr.over f unop_rhs } in
    { get; over }

  and binop_expr =
    let get x = Binop_expr.binop_rhs.get x |> expr.get
    and over f = Binop_expr.binop_rhs.over (expr.over f) in
    { get; over }

  and paren_expr = Paren_expr.paren_close

  and expr =
    let get = function
      | Ref x -> name.get x
      | ECall x -> call.get x
      | Dots x | Nil x | True x | False x -> x
      | Number x -> First.lit_number.get x
      | Int x -> First.lit_int.get x
      | String x -> First.lit_string.get x
      | Fun x -> fun_expr.get x
      | Table x -> table.get x
      | UnOp x -> unop_expr.get x
      | BinOp x -> binop_expr.get x
      | Parens x -> paren_expr.get x
    and over f = function
      | Ref x -> Ref (name.over f x)
      | ECall x -> ECall (call.over f x)
      | Dots x -> Dots (f x)
      | Nil x -> Nil (f x)
      | True x -> True (f x)
      | False x -> False (f x)
      | Number x -> Number (First.lit_number.over f x)
      | Int x -> Int (First.lit_int.over f x)
      | String x -> String (First.lit_string.over f x)
      | Fun x -> Fun (fun_expr.over f x)
      | Table x -> Table (table.over f x)
      | UnOp x -> UnOp (unop_expr.over f x)
      | BinOp x -> BinOp (binop_expr.over f x)
      | Parens x -> Parens (paren_expr.over f x)
    in
    { get; over }

  let table_item =
    let get = function
      | Array x -> expr.get x
      | RawPair { value; _ } -> expr.get value
      | ExprPair { value; _ } -> expr.get value
    and over f = function
      | Array x -> Array (expr.over f x)
      | RawPair ({ value; _ } as rest) -> RawPair { rest with value = expr.over f value }
      | ExprPair ({ value; _ } as rest) -> ExprPair { rest with value = expr.over f value }
    in
    { get; over }

  let assign_stmt = Assign_stmt.assign_vals -| SepList1.first -| expr

  let repeat_stmt = Repeat_stmt.repeat_test -| expr

  let local_stmt =
    let get { local_vars; local_vals; _ } =
      match local_vals with
      | None -> SepList1.last.get local_vars |> var.get
      | Some (_, vs) -> SepList1.last.get vs |> expr.get
    and over f ({ local_vars; local_vals; _ } as rest) =
      match local_vals with
      | None -> { rest with local_vars = SepList1.last.over (var.over f) local_vars }
      | Some (t, vs) -> { rest with local_vals = Some (t, SepList1.last.over (expr.over f) vs) }
    in
    { get; over }

  let return_stmt =
    let get { return_return; return_vals } =
      match return_vals with
      | None -> return_return
      | Some es -> SepList1.last.get es |> expr.get
    and over f { return_return; return_vals } =
      match return_vals with
      | None -> { return_return = f return_return; return_vals }
      | Some es -> { return_return; return_vals = Some (SepList1.last.over (expr.over f) es) }
    in
    { get; over }

  let stmt =
    let get = function
      | Do x -> do_stmt.get x
      | Assign x -> assign_stmt.get x
      | While x -> while_stmt.get x
      | Repeat x -> repeat_stmt.get x
      | ForNum x -> for_num_stmt.get x
      | ForIn x -> for_in_stmt.get x
      | Local x -> local_stmt.get x
      | LocalFunction x -> local_function_stmt.get x
      | AssignFunction x -> function_stmt.get x
      | Return x -> return_stmt.get x
      | If x -> if_stmt.get x
      | Break x -> x
      | SCall x -> call.get x
      | Semicolon x -> x
    and over f = function
      | Do x -> Do (do_stmt.over f x)
      | Assign x -> Assign (assign_stmt.over f x)
      | While x -> While (while_stmt.over f x)
      | Repeat x -> Repeat (repeat_stmt.over f x)
      | ForNum x -> ForNum (for_num_stmt.over f x)
      | ForIn x -> ForIn (for_in_stmt.over f x)
      | Local x -> Local (local_stmt.over f x)
      | LocalFunction x -> LocalFunction (local_function_stmt.over f x)
      | AssignFunction x -> AssignFunction (function_stmt.over f x)
      | Return x -> Return (return_stmt.over f x)
      | If x -> If (if_stmt.over f x)
      | Break x -> Break x
      | SCall x -> SCall (call.over f x)
      | Semicolon x -> Semicolon x
    in
    { get; over }

  let program = Program.eof
end

module Spanned = struct
  open Lens

  let project first last x = Span.of_span2 (first.get x |> Node.span) (last.get x |> Node.span)

  let expr = project First.expr Last.expr

  let name = project First.name Last.name

  let stmt = project First.stmt Last.stmt

  let program = project First.program Last.program

  let table_item = project First.table_item Last.table_item
end
