alias emacs='_emacs -c -n '
is_osx && alias emacs='cocoa_emacs'
alias terminal_emacs='_emacs -t'
export GLOBAL_EMACS=""
is_ssh && emacs="terminal_emacs"

function e {
    [ ! -z "$*" ] && emacs || emacs_open -n "$@"
}

function cocoa_emacs {
    reattach-to-user-namespace zsh -c 'source ~/.zshrc && _emacs -c -n "$@"'
}

function _emacs {
    local server_name="$(_emacs_server_file)"
    if ! emacs_daemon_exists "$server_name"; then
        echo "Starting emacs with server name '$server_name'"
        command emacs --daemon="$server_name"
    fi
    emacsclient "$@" --server-file=$server_name
}

function _emacs_server_file {
    local server_name="$GLOBAL_EMACS"
    [ -z $GLOBAL_EMACS ] && server_name="$(_current_dot_directory)"
    echo $server_name
}

function emacs_daemon_exists {
    ! test -z "$(ps aux | grep -v grep | grep -i "emacs.*--daemon=.*$1$")"
}

function _dot_directory {
    echo $1 | sed "s:/:.:g" 
}

function _current_dot_directory {
    local directory="$(git rev-parse --show-toplevel 2> /dev/null || pwd)"
    _dot_directory $directory
}

function existing_emacs {
    # Return any existing emacs server file or the one that should
    # be created if it doesn't exist.
    local server_file="$(emacs_get_running_instances | head -n1)"
    [ -z "$server_file" ] && server_file="$(_emacs_server_file)"
    echo $server_file
}

function emacs_make_frame_if_none_exists {
    emacsclient -e '(make-frame-if-none-exists-and-focus)' --server-file=$1
    focus_emacs
}

function focus_emacs {
    is_osx && osascript -e 'tell application "Emacs" to activate'
}

function emacs_get_running_instances {
    pgrep -i emacs | xargs ps -o command -p | egrep -o " --daemon=(.*)" | awk -F= '{print $2}' | sed 's/\^J3,4\^J//'
}

function emacs_open {
    if ! emacs_daemon_exists; then
	emacs
    fi
    local server_file="$(emacs_get_running_instances | head -n1)"
    emacs_make_frame_if_none_exists $server_file
    [ ! -z "$*" ] && emacsclient "$@" --server-file="$server_file"
    focus_emacs
}

# Make emacs the default editor.
export EDITOR="zsh -c 'source ~/.zshrc && emacs_open '"'"$@"'
export VISUAL="$EDITOR"
