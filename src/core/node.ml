(** A node is an object which exists in the syntax tree of the program.

    Nodes, along with holding an object, also include additional metadata such as their position and
    surrounding trivia. *)

module Trivia = struct
  type kind =
    | LineComment  (** A short, comment which is terminated by a newline. *)
    | BlockComment  (** A long comment, which may span multiple lines. *)
    | Whitespace  (** Any whitespace, such as spaces, newlines or tabs. *)

  (** A "trivial" part of the program, which is not important for the execution of the program, but
      may be useful for understanding or recreating it. *)
  type t =
    { start : Illuaminate.Position_map.pos;
      kind : kind;
      contents : string
    }

  let make kind contents start = { kind; contents; start }
  let pp out (x : t) = Format.fprintf out "Trivia(%S)" x.contents
  let start t = t.start

  let finish t : Illuaminate.Position_map.pos =
    let (Pos start) = t.start in
    Pos (start + String.length t.contents - 1)

  let span root trivia =
    let open Illuaminate.Lens in
    let (Illuaminate.Position_map.Pos start) = start trivia in
    let (Illuaminate.Position_map.Pos finish) = finish trivia in
    root |> Span.start_offset ^= start |> Span.finish_offset ^= finish
end

(** A node, such as a token or identifier, but with additional metadata.

    Every node has leading and trailing trivia, represented as an array of {!Trivia.t } nodes. *)
type 'a t =
  { leading_trivia : Trivia.t Illuaminate.IArray.t;
    trailing_trivia : Trivia.t Illuaminate.IArray.t;
    contents : 'a;
    span : Span.t  (** The position of the node, not including leading or trailing trivia. *)
  }
[@@deriving show]

(** Update the contents of this node. *)
let with_contents contents (node : _ t) = { node with contents }

(** Get the span of this node, if defined. Otherwise throw an exception. *)
let span node = node.span

open Illuaminate.Lens

(** A lens which exposes the contents of the term. *)
let contents =
  let get (n : _ t) = n.contents in
  let over f (n : _ t) = { n with contents = f n.contents } in
  { get; over }

(** Embed a lens which transforms the whole node with a view on the body. *)
let lens_embed (type s u a b) (inner : (s, u, a, b) lens) : (s t, u t, a t, b t) lens =
  { get = contents %= inner.get;
    over =
      (fun f x ->
        let body = x ^. contents in
        let res = contents.over inner.get x |> f in
        with_contents (inner.over (fun _ -> res ^. contents) body) res)
  }

(** A lens which exposes the trailing trivia of a term.

    When converting a term from a {!SimpleNode} to a {!Node}, we will use the position of the first
    trivial node. *)
let trailing_trivia =
  let get n = n.trailing_trivia in
  let over f n = { n with trailing_trivia = f n.trailing_trivia } in
  { get; over }

(** A lens which exposes the leading trivia of a term.

    When converting a term from a {!SimpleNode} to a {!Node}, we will use the position of the first
    trivial node. *)
let leading_trivia =
  let get n = n.leading_trivia in
  let over f n = { n with leading_trivia = f n.leading_trivia } in
  { get; over }

(** Join two lists of trivial nodes together. While {!(\@)} will normally suffice for this,
    {!join_trivia} attempts to merge whitespace between adjacent nodes too. *)
let join_trivia xs ys : Trivia.t Illuaminate.IArray.t =
  let module IArray = Illuaminate.IArray in
  if IArray.is_empty xs then ys
  else if IArray.is_empty ys then xs
  else
    let is_space = function
      | ' ' | '\t' -> true
      | _ -> false
    in
    match (IArray.last xs, IArray.first ys) with
    (* If our first trivia ends in a space (" ") and the second trivia starts with any whitespace,
       drop that space and just use the leading whitespace. *)
    | { kind = Whitespace; contents = last_c; _ }, { kind = Whitespace; _ }
      when is_space last_c.[String.length last_c - 1] ->
        (* Build an array of xs[:-1] @ ys *)
        let xs_len = IArray.length xs - 1 in
        let out =
          Array.init (xs_len + IArray.length ys) @@ fun i ->
          if i < xs_len then IArray.get xs i else IArray.get ys (i - xs_len)
        in
        IArray.of_array out
    | _ -> IArray.append xs ys
