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
        throws : description list
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
