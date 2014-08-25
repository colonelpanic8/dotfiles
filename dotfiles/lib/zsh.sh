for filename in ~/.lib/zsh/*; do
    source $filename
done

alias srczsh="source ~/.zshrc"

# Online help.
unalias run-help
autoload run-help
HELPDIR=/usr/local/share/zsh/helpfiles
