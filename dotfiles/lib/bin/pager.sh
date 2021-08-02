#!/usr/bin/env sh

if environment_variable_exists INSIDE_EMACS; then
	emacsclient -n "$@"
else
	less -FXr
fi
return 0
