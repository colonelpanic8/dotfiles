# Regular Colors
Black='\e[0;30m'        # Black
Red='\e[0;31m'          # Red
Green='\e[0;32m'        # Green
Yellow='\e[0;33m'       # Yellow
Blue='\e[0;34m'         # Blue
Purple='\e[0;35m'       # Purple
Cyan='\e[0;36m'         # Cyan
White='\e[0;37m'        # White

function current_directory() {
    local PWD=$(pwd)
    echo "${PWD/#$HOME/~}"
}

function current_directory() {
    local PWD=$(pwd)
    echo "${PWD/#$HOME/~}"
}

function git_prompt_info() {
    if test -z $(git branch-or-sha);
    then
        echo ""
    else
        echo " on $(git branch-or-sha)$(git_status_character)"
    fi
}

function git_status_character() {
    if git dirty;
    then
        echo "✘"
    else
        echo "✔"
    fi
}

function sandbox_prompt() {
    if [ ! -z $(sandbox_prompt_info) ];
    then
        echo " with $(colored_sandbox_string)"
    fi
}

PS1="╭─% ${Green}\u ${White}at ${Blue}\h ${White}in \$(current_directory)\$(git_prompt_info)\$(sandbox_prompt)
╰─± "
