source_directory_files "$HOME/.lib/zsh"

alias srczsh="source ~/.zshrc"
alias zshenv="source ~/.zshenv"

# Online help.
unalias run-help 2> /dev/null 1>/dev/null
autoload run-help
HELPDIR=/usr/local/share/zsh/helpfiles

function if_emacs_zsh {
    echo "$INSIDE_EMACS" | grep -q term && export PS1="$ "
}

environment_variable_exists INSIDE_EMACS && export PROMPT='$ '

# Enable direnv
eval "$(direnv hook zsh)"
