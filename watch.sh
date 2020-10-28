#!/usr/bin/env bash

set -ex

EXE_PATH=src/bin/cli/illuaminate.exe
RUN_PATH="_build/default/$EXE_PATH"
dune build "$EXE_PATH"

# fswatch --event=Updated --exclude '#' -o -r ../CC-Tweaked/doc/stub ../CC-Tweaked/src/main/resources/ \
fswatch --event=Updated --exclude '#' -o -r src/ \
| while read p ; do
  # "$RUN_PATH" lint ../CC-Tweaked && "$RUN_PATH" doc-gen ../CC-Tweaked || true
  dune exec src/bin/cli/illuaminate.exe doc-gen ../CC-Tweaked || true
done
