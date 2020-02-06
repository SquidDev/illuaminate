#!/usr/bin/env fish


echo "Initial generation"
dune exec src/bin/illuaminate_all.exe doc-gen ../CC-Tweaked
dune exec src/bin/illuaminate_all.exe lint ../CC-Tweaked

fswatch --exclude '#' --event Updated -or ../CC-Tweaked/doc/stub/ | while read -l line
  echo "Regenerating due to $line changes"
  dune exec src/bin/illuaminate_all.exe lint ../CC-Tweaked
  dune exec src/bin/illuaminate_all.exe doc-gen ../CC-Tweaked
end
