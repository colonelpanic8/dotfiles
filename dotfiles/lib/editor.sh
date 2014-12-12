#!/usr/bin/env zsh
[ -z "$*" ] && emacs || emacs_open "$@"
return 0
