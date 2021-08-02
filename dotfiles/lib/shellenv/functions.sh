function command_exists {
    hash "$1" 2>/dev/null 1>/dev/null
}

function run_if_exists {
	command_exists "$@" && "$@"
}

function shell_contains () {
  local e
  for e in "${@:2}"; do
      [[  "$1" == *"$e"* ]] && return 0
  done
  return 1
}

function environment_variable_exists {
    eval "value=\"\${$1+x}\""
    [ ! -z $value ]
}

function get_python_scripts_path {
    python -c "import sysconfig; print sysconfig.get_path('scripts')"
}

function path_lines {
	IFS=':' read -A ADDR <<< "$PATH"
	for one_path in "${ADDR[@]}"; do
		echo $one_path
	done
}

function indirect_expand {
    eval "value=\"\${$1}\""
    echo $value
}

function exists_in_path_var {
    target=${2-PATH}
    local path_contents="$(indirect_expand $target)"
    [[ ":$path_contents:" == *":$1:"* ]]
}

function split_into_vars {
    local string IFS

    string="$1"
    IFS="$2"
    shift 2
    read -r -- "$@" <<EOF
$string
EOF
}

function echo_split {
   local IFS
   IFS="$2" read -rA -- arr <<EOF
$1
EOF
    for i in "${arr[@]}"; do
        echo $i
    done
}

function shell_contains {
  local e
  for e in "${@:2}"; do
      [[  "$1" == *"$e"* ]] && return 0
  done
  return 1
}

function current_shell {
    which "$(ps -p $$ | tail -1 | awk '{print $NF}' | sed 's/\-//')"
}

function is_zsh {
    [ ! -z ${ZSH_VERSION+x} ]
}

function git_diff_add {
    git status --porcelain | awk '{print $2}' | xargs -I filename sh -c "git du filename && git add filename"
}

function get_cols {
    FS=' '
    OPTIND=1
    while getopts "F:" OPTCHAR; do
        case $OPTCHAR in
            F)
                FS=$OPTARG
                ;;
        esac
    done
    shift $((OPTIND-1))
    gawk -f "$HOME/.lib/get_cols.awk" -v "cols=$*" -v "FS=$FS"
}

function filter_by_column_value {
    awk '$'"$1"' == '"$2"'  { print $0 }'
}

# Start an HTTP server from a directory, optionally specifying the port
function server {
    local port="${1:-8000}"
    sleep 1 && open "http://localhost:${port}/" &
    # Set the default Content-Type to `text/plain` instead of `application/octet-stream`
    # And serve everything as UTF-8 (although not technically correct, this doesn’t break anything for binary files)
    python -c $'import SimpleHTTPServer;\nmap = SimpleHTTPServer.SimpleHTTPRequestHandler.extensions_map;\nmap[""] = "text/plain";\nfor key, value in map.items():\n\tmap[key] = value + ";charset=UTF-8";\nSimpleHTTPServer.test();' "$port"
}

# All the dig info
function digga {
    dig +nocmd "$1" any +multiline +noall +answer
}

function shell_stats() {
    history 0 | awk '{CMD[$2]++;count++;}END { for (a in CMD)print CMD[a] " " CMD[a]/count*100 "% " a;}' | grep -v "./" | column -c3 -s " " -t | sort -nr | nl |  head -n20
}

function git_diff_replacing() {
    local original_sha='HEAD~1'
    local new_sha='HEAD'
    OPTIND=1
    while getopts "do:n:" OPTCHAR;
    do
        case $OPTCHAR in
            o)
                original_sha="$OPTARG"
                ;;
            n)
                new_sha="$OPTARG"
                ;;
            d)
                debug="true"
        esac
    done
    shift $((OPTIND-1))
    local replaced="$1"
    local replacing="$2"
    local replace_sha_string='$(echo filename | sed '"s:$replaced:$replacing:g"')'
    test -z $debug || echo "Diffing from $original_sha to $new_sha, replacing $replaced with $replacing"
    test -z $debug || git diff $original_sha $new_sha --name-only | grep -v "$replacing"
    git diff $original_sha $new_sha --name-only | grep -v "$replacing" | xargs -I filename sh -c "git diff $original_sha:filename $new_sha:"$replace_sha_string
}

function git_reset_author() {
    local should_update_command=''
    local update_command=''
    OPTIND=1
    while getopts "a:e:A:E:h" OPTCHAR;
    do
        case $OPTCHAR in
            a)
                new_author="$OPTARG";
                test -n "$update_command" && update_command="$update_command"' && '
                update_command="$update_command"'export GIT_AUTHOR_NAME='"'$new_author'"' && export GIT_COMMITTER_NAME='"'$new_author'"
                ;;
            A)
                author_regex="$OPTARG";
                test -n "$should_update_command" && should_update_command="$should_update_command"' && '
                should_update_command=$should_update_command'[[ "$GIT_AUTHOR_NAME" =~ "'"$author_regex"'" ]]'
                ;;
            e)
                new_email="$OPTARG";
                test -n "$update_command" && update_command="$update_command"' && '
                update_command="$update_command"'export GIT_AUTHOR_EMAIL='"'$new_email'"' && export GIT_COMMITTER_EMAIL='"'$new_email'"
                ;;
            E)
                email_regex="$OPTARG";
                test -n "$should_update_command" && should_update_command="$should_update_command"' && '
                should_update_command=$should_update_command'[[ "$GIT_AUTHOR_EMAIL" =~ "'"$email_regex"'" ]]'
                ;;
            h)
                echo "Usage:
-a specify the new author/committer name.
-A specify a regex that will be used to filter commits by author name.
-e specify the new author/committer email.
-E specify a regex that will be used to filter commits by author email.
-h show this help message.
"
                return
                ;;
        esac
    done
    local filter_branch_command="$should_update_command"' && '"$update_command"' || test true'
    git filter-branch -f --env-filter $filter_branch_command -- --all
}

alias git_reset_author_to_user='git_reset_author -a "$(git config --get user.name)" -e "$(git config --get user.email)" '
alias git_reset_author_from_user='git_reset_author -A "$(git config --get user.name)" -E "$(git config --get user.email)" '

function git_prune_all_history_involving {
    git filter-branch --force --index-filter \
        "git rm -r --cached --ignore-unmatch $1" \
        --prune-empty --tag-name-filter cat -- --all
}

function pip_package_location() {
    pip show $1 | grep Location | get_cols 2
}

function git_config_string() {
    git config -f $1 --list | xargs -I kv printf '-c \"%s\" ' kv
}

function track_modified {
    local timestamp_file="/tmp/__track_modified_timestamp__"
    touch $timestamp_file
    stat $timestamp_file
    echo "Press any key to execute find command"
    read -r key
    echo "Finding..."
    find $1 -cnewer "$timestamp_file"
}

function python_module_path {
    python -c "import os, $1; print(os.path.dirname($1.__file__))"
}

function timestamp {
    date +%s
}

function parse_timestamp {
    date -d "@$1"
}

function parse_timestamp2 {
    date -d "@$(echo $1 | cut -c -10)" -Iseconds
}

function file_ends_with_newline {
    [[ $(tail -c1 "$1" | wc -l) -gt 0 ]]
}

function source_if_exists {
    test -r "$1" && source "$1"
}

function edit_script {
    $EDITOR "$(which $1)"
}

function which_readlink {
    readlink -f "$(which $1)"
}

function localip {
    case `uname` in
        'Darwin')
            ifconfig | grep -Eo 'inet (addr:)?([0-9]*\.){3}[0-9]*' | grep -Eo '([0-9]*\.){3}[0-9]*' | grep -v '127.0.0.1'
            ;;
        'Linux')
			ip -4 addr | grep -oP '(?<=inet\s)\d+(\.\d+){3}' | grep -v 127.0.0.1 | head -n 1
            ;;
    esac
}

function all_lines_after {
    sed -n "/$1/"'$p'
}

function list_interfaces {
    ip link show | grep -vE '^ ' | get_cols -F ':' 2 | xargs -n 1
}

function all_after_char {
    while read -r line; do
          echo ${line##*$1}
    done;
}

function pasink {
    pacmd stat | awk -F": " '/^Default sink name: /{print $2}'
}

function pavolume {
    pacmd list-sinks |
        awk '/^\s+name: /{indefault = $2 == "<'"$(pasink)"'>"}
            /^\s+volume: / && indefault {print $5; exit}' | grep -Eo "[0-9]*"
}

function paismuted {
	pactl list sinks | grep "$(pasink)" -A 10 | grep Mute | grep -q yes
}

function pashowvolume {
	if paismuted; then
		volnoti-show -m
	else
		volnoti-show "$(min $(pavolume) 100)"
	fi
}

function pashowinputbypid {
	get_sink_input_info.hs | jq 'select(.application_process_id == "'"$1"'")'
}
