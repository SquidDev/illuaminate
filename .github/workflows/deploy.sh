#!/usr/bin/env bash

set -eu

# Setup ssh key
mkdir -p "$HOME/.ssh/"
echo "$SSH_KEY" > "$HOME/.ssh/key"
chmod 600 "$HOME/.ssh/key"

OUT_DIR="$(git describe --always --abbrev=7)"
mkdir "$OUT_DIR"
mv artifacts/*/* "$OUT_DIR"

# And upload
rsync -avc -e "ssh -i $HOME/.ssh/key -o StrictHostKeyChecking=no -p $SSH_PORT" \
      "$GITHUB_WORKSPACE/$OUT_DIR" \
      "$SSH_USER@$SSH_HOST:/var/www/squiddev.cc/illuaminate/bin"
