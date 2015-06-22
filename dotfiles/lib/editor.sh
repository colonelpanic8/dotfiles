#!/usr/bin/env zsh
if is_osx; then
   reattach-to-user-namespace emacsclient "$@"
else
	emacsclient "$@"
fi
	
return 0
