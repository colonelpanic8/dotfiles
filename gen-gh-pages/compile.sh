#!/usr/bin/env bash

export PATH="$HOME/.cask/bin:$HOME/.evm/bin:$PATH"
THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

TARGET=$(readlink -f "$THIS_DIR/../dotfiles/emacs.d/README.org")

git clone https://github.com/rejeep/evm.git "$HOME/.evm"
evm config path /tmp
evm install emacs-25.1-travis --use --skip
export PATH="$HOME/.evm/bin:$PATH"

curl -fsSkL https://raw.github.com/cask/cask/master/go | python
export PATH="/home/travis/.cask/bin:$PATH"

echo "this is PATH $PATH"

cask install
run_make_on_org
cask exec emacs --script generate-html.el

mv "$THIS_DIR/../dotfiles/emacs.d/README.html" .


function run_make_on_org () {
	original="$(pwd)"
	cd .cask
	cd "$(ls | head)"
	cd elpa
	cd "$(ls | grep org-plus)"
	make autoloads
	cd $original
}
