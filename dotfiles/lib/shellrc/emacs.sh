alias emacs='cemacs'
alias cemacs='_emacs -c -n'
alias temacs='_emacs -t'
alias ec='_emacs -n '

function _emacs {
    local server_name="$(_current_dot_directory)"
    if ! _emacs_daemon_exists "$server_name"; then
        echo "Starting emacs with server name '$server_name'"
        \emacs --daemon="$server_name"
    fi
    emacsclient $* --server-file=$server_name
}

function _emacs_daemon_exists {
    ! test -z "$(ps aux | grep -v grep | grep -i "emacs.*--daemon=.*$1$")"
}

function _dot_directory {
    echo $1 | sed "s:/:.:g" 
}

function _current_dot_directory {
    local directory="$(git rev-parse --show-toplevel 2> /dev/null || pwd)"
    _dot_directory $directory
}
