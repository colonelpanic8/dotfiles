## Command history configuration
export HISTFILE="$(readlink -f $HOME/.zsh_history)"
HISTSIZE=10000000
SAVEHIST=10000000

setopt share_history # share command history data
setopt extended_history
setopt inc_append_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
