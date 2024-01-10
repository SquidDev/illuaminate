open IlluaminateCore

type module_kind =
  | MKModule
  | MKLibrary
  | MKNone

type position =
  { path : string;
    start_line : int;
    end_line : int
  }

type source =
  | Span of Span.t
  | Position of position

type change_kind =
  | Added
  | Changed

module Cmarkit_meta = struct
  let admonition_level : Cmarkit.Block.Admonition.level Cmarkit.Meta.key = Cmarkit.Meta.key ()
end

module Comment_lines = struct
  module IntMap = Map.Make (Int)

  type t = Span.t IntMap.t

  let __make x = x

  let span mapping =
    Span.of_span2 (IntMap.min_binding mapping |> snd) (IntMap.max_binding mapping |> snd)

  let span_of_range mapping start finish =
    let start_pos, start_span = IntMap.find_last (fun x -> x <= start) mapping
    and finish_pos, finish_span = IntMap.find_last (fun x -> x <= finish) mapping in
    let open Illuaminate.Lens in
    start_span
    |> Span.start_offset ^= ((start_span ^. Span.start_offset) + start - start_pos)
    |> Span.finish_offset ^= ((finish_span ^. Span.start_offset) + finish - finish_pos)

  let span_of_textloc mapping loc =
    if Cmarkit.Textloc.is_none loc then None
    else
      Some (span_of_range mapping (Cmarkit.Textloc.first_byte loc) (Cmarkit.Textloc.last_byte loc))

  let span_of_meta mapping meta = span_of_textloc mapping (Cmarkit.Meta.textloc meta)
  let span_of_node mapping (_, meta) = span_of_meta mapping meta
end

module type S = sig
  type reference

  module Type : Type_syntax.S with type reference = reference

  module Markdown : sig
    type t = Markdown of Cmarkit.Doc.t [@@unboxed]

    val reference : reference Cmarkit.Meta.key
    val doc : t -> Cmarkit.Doc.t
    val as_single_paragraph : t -> Cmarkit.Inline.t option
    val iter_references : (Cmarkit.Label.t -> reference -> unit) -> t -> unit
  end

  type description =
    { description : Markdown.t;
      description_pos : Comment_lines.t
    }

  type nonrec module_kind = module_kind =
    | MKModule
    | MKLibrary
    | MKNone

  type nonrec change_kind = change_kind =
    | Added
    | Changed

  type see =
    { see_reference : reference;
      see_label : string;
      see_span : Span.t;
      see_description : description option
    }

  type deprecation = { deprecation_message : description option } [@@unboxed]

  type example =
    | RawExample of string Span.spanned
    | RichExample of description

  type opt_arg =
    | Required
    | Optional
    | Default of string

  type arg =
    { arg_name : string;
      arg_opt : opt_arg;
      arg_type : Type.t option;
      arg_description : description option
    }

  type return =
    { ret_type : Type.t option;
      ret_many : bool;
      ret_description : description option
    }

  type nonrec position = position =
    { path : string;
      start_line : int;
      end_line : int
    }

  type change =
    { change_kind : change_kind;
      change_version : string;
      change_span : Span.t;
      change_description : description option
    }

  type changes = change list

  class abstract_iter : object
    method reference : reference -> unit
    method description : description -> unit
    method type_ : Type.t -> unit
    method see : see -> unit
    method deprecation : deprecation -> unit
    method example : example -> unit
    method arg : arg -> unit
    method return : return -> unit
    method change : change -> unit
  end
end

module Make (X : sig
  type reference

  module Type : Type_syntax.S with type reference = reference
end) : S with type reference = X.reference and module Type = X.Type = struct
  type reference = X.reference

  module Type = X.Type

  module Markdown = struct
    type t = Markdown of Cmarkit.Doc.t [@@unboxed]

    let reference : reference Cmarkit.Meta.key = Cmarkit.Meta.key ()
    let doc (Markdown x) = x
    let as_single_paragraph (Markdown x) = Cmarkit_ext.as_single_paragraph x
    let label_reference l = Cmarkit.Label.meta l |> Cmarkit.Meta.find reference

    let iter_references f (Markdown doc) =
      let inline _ () = function
        | Cmarkit.Inline.Link (link, _) ->
            (match Cmarkit.Inline.Link.referenced_label link with
            | Some label -> (
              match label_reference label with
              | Some ref -> f label ref
              | None -> ())
            | None -> ());
            `Default
        | _ -> `Default
      in
      let folder = Cmarkit.Folder.make ~inline () in
      Cmarkit.Folder.fold_doc folder () doc
  end

  type description =
    { description : Markdown.t;
      description_pos : Comment_lines.t
    }

  type nonrec module_kind = module_kind =
    | MKModule
    | MKLibrary
    | MKNone

  type nonrec change_kind = change_kind =
    | Added
    | Changed

  type see =
    { see_reference : reference;
      see_label : string;
      see_span : Span.t;
      see_description : description option
    }

  type deprecation = { deprecation_message : description option } [@@unboxed]

  type example =
    | RawExample of string Span.spanned
    | RichExample of description

  type opt_arg =
    | Required
    | Optional
    | Default of string

  type arg =
    { arg_name : string;
      arg_opt : opt_arg;
      arg_type : Type.t option;
      arg_description : description option
    }

  type return =
    { ret_type : Type.t option;
      ret_many : bool;
      ret_description : description option
    }

  type nonrec position = position =
    { path : string;
      start_line : int;
      end_line : int
    }

  type change =
    { change_kind : change_kind;
      change_version : string;
      change_span : Span.t;
      change_description : description option
    }

  type changes = change list

  class abstract_iter =
    object (self)
      method reference (_ : reference) = ()
      method description (_ : description) = ()
      method type_ (_ : Type.t) = ()

      method see { see_reference; see_label = _; see_span = _; see_description } =
        self#reference see_reference;
        Option.iter self#description see_description

      method deprecation { deprecation_message } = Option.iter self#description deprecation_message

      method example =
        function
        | RawExample _ -> ()
        | RichExample d -> self#description d

      method arg { arg_name = _; arg_opt = _; arg_type; arg_description } =
        Option.iter self#type_ arg_type;
        Option.iter self#description arg_description

      method return { ret_type; ret_many = _; ret_description } =
        Option.iter self#type_ ret_type;
        Option.iter self#description ret_description

      method change { change_description; change_kind = _; change_span = _; change_version = _ } =
        Option.iter self#description change_description
    end
end

module Lift (L : S) (R : S) = struct
  type t =
    { any_ref : L.reference -> string option * R.reference;
      type_ref : L.reference -> R.reference
    }

  let markdown x (L.Markdown.Markdown doc) =
    let module Link = Cmarkit.Inline.Link in
    let inline _ = function
      | Cmarkit.Inline.Link (link, node) -> (
        match Link.reference link with
        | `Inline _ -> Cmarkit.Mapper.default
        | `Ref (layout, target, def) -> (
            let meta = Cmarkit.Label.meta def in
            match Cmarkit.Meta.find L.Markdown.reference meta with
            | Some r ->
                (* Remap the reference and update the backing definition map. *)
                let name, r = x.any_ref r in
                (* If we've got a custom name for this reference, use that as the link text (if none
                   is already present). *)
                let layout, inline =
                  match (name, layout) with
                  | Some name, (`Collapsed | `Shortcut) ->
                      (`Full, Cmarkit.Inline.Text (name, Cmarkit.Meta.none))
                  | _ -> (layout, Link.text link)
                in
                (* Then recreate the link. *)
                let meta =
                  Cmarkit.Meta.remove L.Markdown.reference meta
                  |> Cmarkit.Meta.add R.Markdown.reference r
                in
                let def = Cmarkit.Label.with_meta meta def in
                let link = Link.make inline (`Ref (layout, target, def)) in
                Cmarkit.Mapper.ret (Cmarkit.Inline.Link (link, node))
            | _ -> Cmarkit.Mapper.default))
      | _ -> Cmarkit.Mapper.default
    in
    let mapper = Cmarkit.Mapper.make ~inline () in
    R.Markdown.Markdown (Cmarkit.Mapper.map_doc mapper doc)

  let description x { L.description; description_pos } =
    { R.description = markdown x description; description_pos }

  let example lift : L.example -> R.example = function
    | RawExample e -> RawExample e
    | RichExample d -> RichExample (description lift d)

  let see lift { L.see_reference; see_label; see_span; see_description } =
    let see_label, see_reference =
      match lift.any_ref see_reference with
      | Some label, ref -> (label, ref)
      | None, ref -> (see_label, ref)
    in
    { R.see_reference;
      see_label;
      see_span;
      see_description = Option.map (description lift) see_description
    }

  let deprecation lift { L.deprecation_message } =
    { R.deprecation_message = Option.map (description lift) deprecation_message }

  module Ty_lift = Type_syntax.Lift (L.Type) (R.Type)

  let ty x = Ty_lift.t x.type_ref

  let opt_arg : L.opt_arg -> R.opt_arg = function
    | Required -> Required
    | Optional -> Optional
    | Default x -> Default x

  let arg lift { L.arg_name; arg_opt; arg_type; arg_description } =
    { R.arg_name;
      arg_opt = opt_arg arg_opt;
      arg_type = Option.map (ty lift) arg_type;
      arg_description = Option.map (description lift) arg_description
    }

  let return lift { L.ret_type; ret_many; ret_description } =
    { R.ret_type = Option.map (ty lift) ret_type;
      ret_many;
      ret_description = Option.map (description lift) ret_description
    }

  let change lift { L.change_kind; change_version; change_span; change_description } =
    { R.change_kind;
      change_version;
      change_span;
      change_description = Option.map (description lift) change_description
    }
end
