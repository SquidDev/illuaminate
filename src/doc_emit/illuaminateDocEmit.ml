module Html = struct
  module Options = Html_options
  module Highlight = Html_highlight
  include Html_main
  include Html_loader

  let embedded_js = Html_embedded_scripts.contents
  let embedded_css = Html_embedded_styles.contents
end

module Summary = Summary
