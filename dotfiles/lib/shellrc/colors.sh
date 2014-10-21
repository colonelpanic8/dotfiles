dircolors_file="$HOME/.dircolors"

[ ! -z "$SHELL" ] && test -r $dircolors_files  && eval "$(dircolors $dircolors_file)"
