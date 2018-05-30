#!/usr/bin/env bash
THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DOTFILES_DIR=$THIS_DIR/dotfiles
export DOTFILES_DIR

ls $DOTFILES_DIR | grep -Ev '^\.' | xargs -I thestowtarget sh -c 'stow -d $DOTFILES_DIR -S thestowtarget -t $HOME'
