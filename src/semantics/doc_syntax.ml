open IlluaminateCore

type reference = Reference.resolved

include Doc_abstract_syntax.Make (struct
  type nonrec reference = reference

  module Type = Type_syntax.Resolved
end)

type 'a documented =
  { description : description option;
    descriptor : 'a;
    definition : Span.t;
    examples : example list;
    see : see list;
    local : bool
  }

type value =
  | Function of
      { args : arg list list;
        rets : return list list;
        throws : description list;
        has_self : bool
      }
  | Table of (string * value documented) list
  | Expr of
      { ty : Type.t;
        value : string option
      }
  | Type of type_info
  | Unknown
  | Undefined

and member =
  { member_name : string;
    member_is_method : bool;
    member_value : value documented
  }

and type_info =
  { type_name : string;
    type_members : member list
  }

and module_info =
  { mod_name : string;
    mod_types : type_info documented list;
    mod_kind : module_kind;
    mod_contents : value
  }

let get_suffix = function
  | Function { args = []; _ } -> "()"
  | Function { args = [ args ]; _ } ->
      let buf = Buffer.create 10 in
      Buffer.add_char buf '(';
      let _, opt =
        List.fold_left
          (fun (i, o) { arg_name; arg_opt; _ } ->
            if arg_opt then Buffer.add_char buf '[';
            if i > 0 then Buffer.add_string buf ", ";
            Buffer.add_string buf arg_name;
            (i + 1, o + if arg_opt then 1 else 0))
          (0, 0) args
      in
      for _ = 1 to opt do
        Buffer.add_char buf ']'
      done;
      Buffer.add_char buf ')';
      Buffer.contents buf
  | Function _ -> "(...)"
  | Expr { value = Some value; _ } -> " = " ^ value
  | _ -> ""

let is_documented = function
  | { description = None; examples = []; see = []; _ } -> false
  | _ -> true

let base_iter = new abstract_iter

class iter =
  object (self)
    method abstract_syntax : span:Span.t -> abstract_iter = fun ~span:_ -> base_iter

    method value ~span =
      function
      | Unknown | Undefined -> ()
      | Function { args; rets; throws; has_self = _ } ->
          let abs = self#abstract_syntax ~span in
          List.iter (List.iter abs#arg) args;
          List.iter (List.iter abs#return) rets;
          List.iter abs#description throws
      | Table xs -> List.iter (fun (_, v) -> self#documented self#value v) xs
      | Type t -> self#type_info ~span t
      | Expr { ty; value = _ } -> (self#abstract_syntax ~span)#type_ ty

    method documented : 'a. (span:Span.t -> 'a -> unit) -> 'a documented -> unit =
      fun child { description; descriptor; definition; examples; see; local = _ } ->
        let abs = self#abstract_syntax ~span:definition in
        Option.iter abs#description description;
        child ~span:definition descriptor;
        List.iter abs#example examples;
        List.iter abs#see see;
        ()

    method member { member_name = _; member_is_method = _; member_value; _ } =
      self#documented self#value member_value

    method type_info : span:Span.t -> type_info -> unit =
      fun ~span:_ { type_members; type_name = _ } -> List.iter self#member type_members

    method module_info ~span { mod_name = _; mod_types; mod_kind = _; mod_contents } =
      List.iter (self#documented self#type_info) mod_types;
      self#value ~span mod_contents
  end

let iter_of f =
  object
    inherit iter

    method! abstract_syntax = f
  end
