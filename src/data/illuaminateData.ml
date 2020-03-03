include Core

module Container = struct
  module type S = Contained_tbl.KeyContainer

  module Strong = Contained_tbl.StrongContainer
  module Weak = Contained_tbl.WeakContainer
end

module Programs = Programs
