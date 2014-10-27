alias emacs='cemacs'
alias cemacs='_emacs -c -n'
alias temacs='_emacs -t'
alias ec='_emacs -n '

function _emacs {
    local directory="$(git rev-parse --show-toplevel 2> /dev/null || pwd)"
    local server_name="$(_dot_directory $directory)"
    test -z "$(ps aux | grep -v grep | grep -i "emacs -nw --daemon=.*$server_name$")" &&  echo "Starting emacs with server name '$server_name'" && \emacs --daemon="$server_name"
    emacsclient $* --server-file="$server_name"
}

function _dot_directory {
    echo $1 | sed "s:/:.:g" 
}
