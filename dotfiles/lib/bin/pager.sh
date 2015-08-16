#!/usr/bin/env zsh
if environment_variable_exists INSIDE_EMACS; then
	emacs_pager "$@"
else
	less -FXr
fi
return 0
