open IlluaminateSemantics.Doc.AbstractSyntax

type t =
  { site_title : string option;
    site_image : string option;
    site_url : string option;
    site_head : string option;
    site_css : string;
    site_js : string;
    data : IlluaminateData.t;
    resolve : string -> string;
    source_link : source -> string option;
    custom : IlluaminateSemantics.Doc.Extract.Config.custom_kind list
  }

let make ?site_title ?site_image ?site_url ?site_head ~site_css ~site_js ~resolve ~data ?source_link
    ?(custom = []) () =
  { site_title;
    site_image;
    site_url;
    site_head;
    site_css;
    site_js;
    resolve;
    data;
    source_link = Option.value ~default:(Fun.const None) source_link;
    custom
  }
