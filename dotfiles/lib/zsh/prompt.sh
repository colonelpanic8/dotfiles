autoload -U colors && colors
function current_directory() {
    local PWD=$(pwd)
    echo "${PWD/#$HOME/~}"
}

function git_prompt_info() {
    if test -z $(git branch-or-sha);
    then
        echo ""
    else
        echo " %{$FG[239]%}on%{$reset_color%} %{$FG[255]%}$(git branch-or-sha)%{$reset_color%}$(git_status_character)"
    fi
}

function git_status_character() {
    if git dirty;
    then
        echo "%{$FG[202]%}✘%{$reset_color%}"
    else
        echo "%{$FG[040]%}✔%{$reset_color%}"
    fi
}

function sandbox_prompt() {
    if [ ! -z "$(sandbox_prompt_info)" ];
    then
        echo " %{$FG[239]%}with $(colored_sandbox_string)%{$reset_color%}"
    fi
}

PROMPT='╭─% %{$FG[040]%}%n%{$reset_color%} %{$FG[239]%}at%{$reset_color%} %{$FG[033]%}$(hostname -s)%{$reset_color%} %{$FG[239]%}in%{$reset_color%} %{$terminfo[bold]$FG[226]%}$(current_directory)%{$reset_color%}$(git_prompt_info)$(sandbox_prompt)
$FG[255]%{$reset_color%}╰─± '

PS2=''

RPROMPT='Last Exit Code: $?'
