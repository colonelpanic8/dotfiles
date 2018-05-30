# This is necessary for tramp.
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
source ~/.lib/shellrc.sh
source ~/.lib/zsh.sh
test -r ~/.custom.sh && source ~/.custom.sh
source_if_exists ~/.this-machine.sh
