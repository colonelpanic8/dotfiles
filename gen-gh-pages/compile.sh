#!/usr/bin/env bash

export PATH="$HOME/.cask/bin:$HOME/.evm/bin:$PATH"
THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

TARGET=$(readlink -f "$THIS_DIR/../dotfiles/emacs.d/README.org")

git clone https://github.com/rejeep/evm.git "$HOME/.evm"
evm config path /tmp
evm install emacs-25.1-travis --use --skip
export EMACS="$(evm bin)"

curl -fsSkL https://raw.github.com/cask/cask/master/go | python

cask install
cask exec emacs --script generate-html.el

mv "$THIS_DIR/../dotfiles/emacs.d/README.html" .

