# ATTENTION: Most of the functions in this file are part of a now
# deprecated system for starting emacs that made the $EDITOR
# environment variable work for git and other things


# This approach is no longer in use.
# alias emacs='_emacs -c -n '
# is_osx && alias emacs='cocoa_emacs'
# alias terminal_emacs='_emacs -t'
# is_ssh && emacs="terminal_emacs"

alias emacs='emacsclient -t -n'
alias e='emacsclient -n'
is_osx && alias emacs='reattach-to-user-namespace emacsclient -c -n'

function kill_all_emacs {
    emacs_pids | xargs kill -9
}

function edb {
    \emacs --debug-init
}

function cocoa_emacs {
    reattach-to-user-namespace zsh -c '_emacs -c -n "$@"'
}

function _emacs_daemon {
    local server_name="$(_emacs_server_file)"
    if ! emacs_daemon_exists "$server_name"; then
        echo "Starting emacs with server name '$server_name'"
        command emacs "$@" --daemon="$server_name"
    fi
}

function _emacs {
    _emacs_daemon
    emacsclient "$@" --server-file="$(_emacs_server_file)"
}

function _emacs_daemon_arguments {
    _emacs_daemon "$@"
    emacsclient -c -n --server-file="$(_emacs_server_file)"
}

function _emacs_server_file {
    local server_name="global"
    [ ! -z ${PER_DIRECTORY_EMACS+y} ] && server_name="$(_current_dot_directory)"
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
    execute_elisp '(make-frame-if-none-exists-and-focus)' > /dev/null
    focus_emacs
}

function emacs_ensure_running_with_frame {
    test -z "$(emacs_get_running_instances)" && emacs || \
	    emacs_make_frame_if_none_exists "$(emacs_get_running_instances | head -n1)"
}

function execute_elisp {
    _emacs -e "$1"
}

function focus_emacs {
    is_osx && osascript -e 'tell application "Emacs" to activate'
}

function emacs_pids {
    local flags=''
    is_osx && flags='-i'
    pgrep "$flags" emacs
}

function emacs_get_running_instances {
    emacs_pids | xargs ps -o command -p | egrep -o " --daemon=(.*)" | awk -F= '{print $2}' | sed 's/\^J3,4\^J//'
}

function emacs_open {
    if ! emacs_daemon_exists; then
	emacs
    fi
    emacs_make_frame_if_none_exists
    [ ! -z "$*" ] && _emacs "$@"
    focus_emacs
}

function time_emacs {
    time \emacs --daemon="timing" && emacsclient -e "(kill-emacs)" --server-file="timing"
}

function emacs_editor {
    local client_path="$(which emacsclient)"
	if is_osx; then
		reattach-to-user-namespace "$client_path" "$@"
	else
		"$client_path" "$@"
	fi
}

function emacs_pager {
    TMP="$(mktemp -t emacs_pager.XXXXX --suffix=.ansi_color)"
    echo $TMP
    cat > "$TMP"
    # -n may seem strange here, but it actually makes sense. There is
    # -no need to wait since the buffer is inside of emacs now
    emacs_editor "$TMP" -n "$@"
    rm "$TMP"
}

# Make emacs the default editor.
export EDITOR="$HOME/.lib/bin/editor.sh"
export ALTERNATE_EDITOR=""
export VISUAL="$EDITOR"
export GIT_EDITOR="$EDITOR"

# This actually gets executed in dotfiles/lib/shellrc.sh to make sure that it takes precedence over other settings
function inside_emacs_hook {
    export PAGER="$HOME/.lib/bin/pager.sh"
    export GITPAGER="$PAGER"
    export MANPAGER="$PAGER"
}
