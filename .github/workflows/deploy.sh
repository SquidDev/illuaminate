#!/usr/bin/env bash

set -eu

# Setup ssh key
mkdir -p "$HOME/.ssh/"
echo "$SSH_KEY" > "$HOME/.ssh/key"
chmod 600 "$HOME/.ssh/key"

# And upload
rsync -avc -e "ssh -i $HOME/.ssh/key -o StrictHostKeyChecking=no -p $SSH_PORT" \
      "$GITHUB_WORKSPACE/bin" \
      "$SSH_USER@$SSH_HOST:/var/www/squiddev.cc/illuaminate/windows-x86-64/"
