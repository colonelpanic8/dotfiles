## Command history configuration
HISTFILE="$(readlink -f $HOME/.zsh_history)"
HISTSIZE=100000
SAVEHIST=100000

setopt extended_history
setopt hist_expire_dups_first
setopt hist_ignore_dups # ignore duplication command history list
setopt hist_ignore_space
setopt hist_verify
setopt share_history # share command history data

# TODO: Explain what some of this does..

bindkey -e
bindkey '\ew' kill-region
bindkey -s '\el' "ls\n"
bindkey '^r' history-incremental-search-backward
bindkey "^[[5~" up-line-or-history
bindkey "^[[6~" down-line-or-history
