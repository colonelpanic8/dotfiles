function current_shell() {
    readlink -f $(which "$(ps -p $$ | tail -1 | awk '{print $NF}' | sed 's/\-//')")
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

function ospaste() {
    if is_osx;
    then
        reattach-to-user-namespace pbpaste
    fi
}

function git_root() {
    cd `git root`
}

function git_diff_replacing() {
    local original_sha='HEAD~1'
    local new_sha='HEAD'
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

function set_osx_hostname() {
    local new_hostname="${1-imalison}"
    sudo scutil --set ComputerName $new_hostname
    sudo scutil --set HostName $new_hostname
    sudo scutil --set LocalHostName $new_hostname
    sudo defaults write /Library/Preferences/SystemConfiguration/com.apple.smb.server NetBIOSName -string $new_hostname
}

function fix_brew_htop() {
    chmod 777 $(readlink -f `which htop`)
    sudo chown root $(readlink -f `which htop`)
    sudo chmod 6555 `which htop`
}

function pip_package_location() {
    pip show $1 | grep Location | get_cols 2
}

function make_ensime() {
    echo '\n\naddSbtPlugin("org.ensime" % "ensime-sbt-cmd" % "0.1.1")' >> project/plugins.sbt
    sbt "ensime generate"
}

function set_modifier_keys_for_vendor_product_id() {
    defaults -currentHost write -g com.apple.keyboard.modifiermapping.$1-0 '(
{
  HIDKeyboardModifierMappingDst = 2;
  HIDKeyboardModifierMappingSrc = 0;
})'
}

function set_modifier_keys_on_all_keyboards() {
    for vendor_product_id in $(get_keyboard_vendor_id_product_id_pairs | tr " " "-"); do set_modifier_keys_for_vendor_product_id $vendor_product_id; echo $vendor_product_id; done;
}

function get_keyboard_vendor_id_product_id_pairs() {
    ioreg -n IOHIDKeyboard -r | grep -e 'class IOHIDKeyboard' -e VendorID\" -e Product | gawk 'BEGIN { RS = "class IOHIDKeyboard" } match($0, /VendorID. = ([0-9]*)/, arr) { printf arr[1]} match($0, /ProductID. = ([0-9]*)/, arr) { printf " %s\n", arr[1]} '
}

function make_me_synergy() {
    local new_host_name="$(echo $SSH_CONNECTION | get_cols 1)"
    while getopts "h:" OPTCHAR; do
        case $OPTCHAR in
            h)
                new_host_name="$OPTARG";
                return
                ;;
        esac
    done
    synergyc $new_host_name
}

function stop_synergy_at() {
    pgrep synergys | xargs kill
    ssh $1 "source ~/.zshrc && clear_my_synergy"
}

function clear_all_synergy() {
    pgrep synergy | xargs kill
}

function clear_my_synergy() {
    clear_synergy_for_ip "$(echo $SSH_CONNECTION | get_cols 1)"
}

function clear_synergy_for_ip() {
    ps aux | grep -e synergyc | grep $1 | get_cols 2 | xargs kill
}

function activate_synergy_for() {
    synergys --config ~/synergy.conf && ssh $1 "source ~/.zshrc && make_me_synergy"
}
