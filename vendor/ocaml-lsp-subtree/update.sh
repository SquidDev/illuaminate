#!/usr/bin/env sh

set -xe

if [ ! -d "ocaml-lsp" ]; then
  git clone https://github.com/ocaml/ocaml-lsp.git
else
  git -C ocaml-lsp pull -q
fi

rm -rf lsp-fiber jsonrpc-fiber
cp -r ocaml-lsp/LICENSE.md .
cp -r ocaml-lsp/lsp-fiber/src lsp-fiber
cp -r ocaml-lsp/jsonrpc-fiber/src jsonrpc-fiber
