function parse_git_branch() {
    ref=$(git symbolic-ref HEAD 2> /dev/null) || \
    ref=$(git rev-parse --short HEAD 2> /dev/null) || return
    echo ${ref#refs/heads/}
}

function git_is_dirty() {
    ! test -z "$(git status --porcelain)"
}

function current_shell() {
    greadlink -f $(which "$(ps -p $$ | tail -1 | awk '{print $NF}' | sed 's/\-//')")
}

function is_zsh() {
    test -n "$(current_shell | grep -o zsh)"
}

function git_diff_add() {
    git status --porcelain | awk '{print $2}' | xargs -I filename sh -c "git du filename && git add filename"
}

function confirm() {
    # call with a prompt string or use a default
    read -r -p "$1" response
    case $response in
        [yY][eE][sS]|[yY]) 
            return 0
            ;;
        *)
            return 1
            ;;
    esac
}

function get_cols() {
    FS=' '
    while getopts "F:" OPTCHAR; do
        case $OPTCHAR in
            F)
                FS=$OPTARG
                ;;
        esac
    done
    shift $((OPTIND-1))
    awk -f "$HOME/.lib/get_cols.awk" -v "cols=$*" -v "FS=$FS"
}

function find_all_ssh_agent_sockets() {
    find /tmp -type s -name agent.\* 2> /dev/null | grep '/tmp/ssh-.*/agent.*'
}

function set_ssh_agent_socket() {
    export SSH_AUTH_SOCK=$(find_all_ssh_agent_sockets | tail -n 1 | awk -F: '{print $1}')
}

# Determine size of a file or total size of a directory
function fs() {
    if du -b /dev/null > /dev/null 2>&1; then
	local arg=-sbh
    else
	local arg=-sh
    fi
    if [[ -n "$@" ]]; then
	du $arg -- "$@"
    else
	du $arg .[^.]* *
    fi
}

# Start an HTTP server from a directory, optionally specifying the port
function server() {
    local port="${1:-8000}"
    sleep 1 && open "http://localhost:${port}/" &
	# Set the default Content-Type to `text/plain` instead of `application/octet-stream`
	# And serve everything as UTF-8 (although not technically correct, this doesn’t break anything for binary files)
    python -c $'import SimpleHTTPServer;\nmap = SimpleHTTPServer.SimpleHTTPRequestHandler.extensions_map;\nmap[""] = "text/plain";\nfor key, value in map.items():\n\tmap[key] = value + ";charset=UTF-8";\nSimpleHTTPServer.test();' "$port"
}

# All the dig info
function digga() {
    dig +nocmd "$1" any +multiline +noall +answer
}

function shell_stats() {
    history | awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' | grep -v "./" | column -c3 -s " " -t | sort -nr | nl |  head -n20
}

function is_ssh() {
    test $SSH_CLIENT
}

function is_osx() {
    case `uname` in
        'Darwin')
            return 0
	    ;;
        *)
            return 1;
            ;;
    esac
}

function clipboard() {
    if is_osx;
    then
        reattach-to-user-namespace pbcopy
    fi
}
