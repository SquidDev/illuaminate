module StringMap = Map.Make (String)

type attribute =
  | Default
  | Unused
  | Deprecated

let tag_of_attribute : attribute -> Illuaminate.Error.tag option = function
  | Default -> None
  | Unused -> Some Unneccessary
  | Deprecated -> Some Deprecated

type level = Illuaminate.Error.severity =
  | Error
  | Warning
  | Note

module Tag = struct
  type t =
    { name : string;
      level : level;
      attributes : attribute list
    }

  let pp f { name; _ } = Format.fprintf f "%s" name
  let tags = ref StringMap.empty

  let make ~attr ~level name =
    let tag = { level; name; attributes = attr } in
    tags := StringMap.add name tag !tags;
    tag

  let find name = StringMap.find_opt name !tags
  let compare l r = String.compare l.name r.name
  let has a l = List.mem a l.attributes

  type filter = t -> bool
end
