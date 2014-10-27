alias emacs='_emacs -c'
alias cemacs'_emacs -c'
alias temacs'_emacs -t'

function _emacs {
    test -z "$(ps aux | grep -v grep | grep -i "\emacs -nw --daemon=.*`dotted_directory`")" && \emacs --daemon="$(dotted_directory)"
    emacsclient $* -n --server-file="$(dotted_directory)"
}

function dotted_directory {
    pwd | sed "s:/:.:g"
}
