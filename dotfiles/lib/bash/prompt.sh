function current_directory() {
    local PWD=$(pwd)
    echo "${PWD/#$HOME/~}"
}

function git_prompt_info () {
    if test -z $(parse_git_branch);
    then
        echo ""
    else
        echo "on $(parse_git_branch)"
    fi
}

PS1='%u at $(hostname -s) in $(current_directory) $(git_prompt_info) $(colored_sandbox_string) '
