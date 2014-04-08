for filename in ~/.lib/zsh/*; do
    source $filename
done

CASE_SENSITIVE="true"
fpath=(~/.lib/completions $fpath)
autoload -U compinit
compinit
# Allow command line editing.
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line
setopt PROMPT_SUBST
