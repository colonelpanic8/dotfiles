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

function prompt_use_custom_colors() {
    export USERNAME_COLOR="$FG[040]"
    export SEPARATOR_COLOR="$FG[239]"
    export HOSTNAME_COLOR="$FG[033]"
    export CURRENT_DIRECTORY_COLOR="$FG[226]"
}

function prompt_use_basic_colors() {
    export USERNAME_COLOR="$fg[blue]"
    export SEPARATOR_COLOR="$fg[black]"
    export HOSTNAME_COLOR="$fg[green]"
    export CURRENT_DIRECTORY_COLOR="$fg[yellow]"
}

function prompt_grey_separator() {
    export USERNAME_COLOR="$fg[blue]"
    export SEPARATOR_COLOR="$FG[239]"
    export HOSTNAME_COLOR="$fg[green]"
    export CURRENT_DIRECTORY_COLOR="$fg[yellow]"
}

function print_with_color() {
    echo "%{$2%}$1%{$reset_color%}"
}

function separator() {
    print_with_color "$1" "$SEPARATOR_COLOR"
}

PROMPT='╭─% $(print_with_color "%n" "$USERNAME_COLOR") $(separator "at") $(print_with_color "`hostname -s`" "$HOSTNAME_COLOR") $(separator "in") $(print_with_color "`current_directory`" "$terminfo[bold]$CURRENT_DIRECTORY_COLOR")$(git_prompt_info)$(sandbox_prompt)
╰─± '

PS2='(%_) '

RPROMPT='Last Exit Code: $?'
prompt_use_custom_colors
