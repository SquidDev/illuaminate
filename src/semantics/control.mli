open IlluaminateCore

(** The source of a jump between two basic blocks. *)
type edge_source = private
  | Jump of Syntax.stmt
      (** This jump originated from a statement, such as a [break] or [if] node. *)
  | Fallthrough  (** This jump originated from falling off the end of a block. *)

type block_contents = private
  | Test of Syntax.expr  (** This basic block is composed of a test expression. *)
  | TestFor of Syntax.stmt  (** The basic block is the test of a for loop of some kind. *)
  | Block of Syntax.block
      (** This basic block is the entry point into this syntactic block.

          Note, this does not mean that it runs all statements within the block.*)
  | LoopEnd of Syntax.stmt
      (** This is the "end point" of the loop, before it continues to the test again. *)

(** An edge from one node to another. *)
type edge = private
  { from_source : edge_source;  (** The source of this edge. *)
    from_block : basic_block;  (** The block this edge comes from. *)
    to_block : basic_block;  (** The block this edge goes to. *)
    backwards : bool
        (** If this is a backwards/reverse jump. Namely, does this go back to the start of a loop? *)
  }

and basic_block = private
  { block_id : int;
        (** The unique identifier of this block. This has no meaning beyond determining equality. *)
    func : func;  (** The function this block belongs to. *)
    mutable incoming : edge list;  (** All edges which enter this block. *)
    mutable outgoing : edge list;  (** All edges which leave this block. *)
    contents : block_contents  (** The contents of this block. *)
  }

and func = private
  { func_id : int;
        (** The unique identifier of this function. This has mo meaning beyond determining
            equality. *)
    entry : basic_block;  (** The entry block for this function. *)
    mutable blocks : basic_block list  (** All blocks within this function. *)
  }

(** The state of control flow analysis for this program. *)
type t

(** A data key, used to look up control flow information. *)
val key : t Data.key

(** Get the top-level function. *)
val get_program : t -> func

(** Get function information from a function definition.

    While this is a little weird, it's the only shared term across all function arguments. *)
val get_func : Syntax.args -> t -> func

(** Get the basic block this statement belongs to. *)
val get_block : Syntax.stmt -> t -> basic_block
