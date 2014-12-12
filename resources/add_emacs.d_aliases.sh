#!/usr/bin/env zsh
git config alias.ped "subtree push --prefix=dotfiles/emacs.d emacs.d master"
git config alias.ued "subtree pull --prefix=dotfiles/emacs.d emacs.d master"
git remote add emacs.d git@github.com:IvanMalison/dotfiles.git
