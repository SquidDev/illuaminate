-- config: (lint (only (syntax:method-name)))

function x:y:z() end
function x:y.z() end
function x.y:z() end --ok
