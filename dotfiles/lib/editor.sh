#!/usr/bin/env zsh

function editor {
	if is_osx; then
		reattach-to-user-namespace emacsclient "$@"
	else
		emacsclient "$@"
	fi
}


if [ -t 1 ]; then
	TMP="$(mktemp -t emacs)"
	echo $TMP
	cat > "$TMP"
	editor "$TMP" "$@"
	rm "$TMP"
else
	editor "$@"
fi
	
return 0
