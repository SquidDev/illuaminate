-- config: (lint (only syntax:redundant-semicolon))
local function f() end

do ; end

-- FIXME: We need to preserve trivia!
do local x; end

do ; local x end

do f(); (f)() end

do f(); f() end
