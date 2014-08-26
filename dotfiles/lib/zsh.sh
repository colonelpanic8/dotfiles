for filename in ~/.lib/zsh/*; do
    source $filename
done

alias srczsh="source ~/.zshrc"

# Online help.
unalias run-help 2> /dev/null 1>/dev/null
autoload run-help
HELPDIR=/usr/local/share/zsh/helpfiles
