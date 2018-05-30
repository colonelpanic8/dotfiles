#!/usr/bin/env bash

export PATH="$HOME/.cask/bin:$HOME/.evm/bin:$PATH"
THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
EMACS_DIR=$(readlink -f "$THIS_DIR/../dotfiles/emacs/.emacs.d/")

TARGET="$EMACS_DIR/README.org"

git clone https://github.com/rejeep/evm.git "$HOME/.evm"
evm config path /tmp
evm install emacs-25.1-travis --use --skip
export EMACS="$(evm bin)"

curl -fsSkL https://raw.github.com/cask/cask/master/go | python

cask install
cask exec "$EMACS" --script generate-html.el

mv "$EMACS_DIR/README.html" .

