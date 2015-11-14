##### COLOR SETUP
autoload -U colors && colors
# P.C. Shyamshankar <sykora@lucentbeing.com>
# Copied from
# http://github.com/sykora/etc/blob/master/zsh/functions/spectrum/
typeset -Ag FX FG BG
FX=(
    reset     "%{[00m%}"
    bold      "%{[01m%}" no-bold      "%{[22m%}"
    italic    "%{[03m%}" no-italic    "%{[23m%}"
    underline "%{[04m%}" no-underline "%{[24m%}"
    blink     "%{[05m%}" no-blink     "%{[25m%}"
    reverse   "%{[07m%}" no-reverse   "%{[27m%}"
)

for color in {000..255}; do
    FG[$color]="%{[38;5;${color}m%}"
    BG[$color]="%{[48;5;${color}m%}"
done

# Show all 256 colors with color number
function spectrum_ls {
  for code in {000..255}; do
    print -P -- "$code: %{$FG[$code]Test%f%}"
  done
}
#####

function current_directory {
    local PWD=$(pwd)
    echo "${PWD/#$HOME/~}"
}

function git_prompt_info {
    if test -z $(git branch-or-sha);
    then
        echo ""
    else
        local b="$(print_with_color $(git branch-or-sha) $SOURCE_CONTROL_COLOR)"
        echo " $(separator "on") $b$(git_status_character)"
    fi
}

function git_status_character {
    if git dirty;
    then
        print_with_color "✘" "$fg[red]"
    else
        print_with_color "✔" "$fg[green]"
    fi
}

function command_line_character {
    if ! test -z $(git branch-or-sha);
    then 
        echo "±"
    else
        echo "○"
    fi
}

function job_count {
    jobs -s | wc -l
}

function colored_job_count {
    local job_count="$(job_count)"
    if [ $job_count -gt 0 ]; then
	print_with_color "($job_count) " $JOB_COUNT_COLOR
    fi
}

export PROMPT_CHAR_ERROR="$fg[red]"
export PROMPT_CHAR_SUCCESS="$fg[green]"

function prompt_custom_colors {
    export USERNAME_COLOR="$FG[040]"
    export SEPARATOR_COLOR="$FG[239]"
    export HOSTNAME_COLOR="$FG[033]"
    export CURRENT_DIRECTORY_COLOR="$FG[226]"
    export CURRENT_DIRECTORY_COLOR="$FG[255]"
}

function prompt_basic_colors {
    export USERNAME_COLOR="$fg_no_bold[green]"
    export SEPARATOR_COLOR="$fg_no_bold[black]"
    export HOSTNAME_COLOR="$fg_no_bold[blue]"
    export CURRENT_DIRECTORY_COLOR="$fg[yellow]"
    export SOURCE_CONTROL_COLOR="$fg[white]"
}

function prompt_solarized_colors {
    prompt_basic_colors
    export HOSTNAME_COLOR="$fg[magenta]"
    export USERNAME_COLOR="$fg[blue]"
    export CURRENT_DIRECTORY_COLOR="$fg[red]"
    export SOURCE_CONTROL_COLOR="$fg[white]"
}

function prompt_tomorrow_colors {
    export SEPARATOR_COLOR="$fg[cyan]"
    export HOSTNAME_COLOR="$fg[yellow]"
    export USERNAME_COLOR="$fg[blue]"
    export CURRENT_DIRECTORY_COLOR="$fg[red]"
    export SOURCE_CONTROL_COLOR="$fg[white]"
}

function prompt_monokai_colors {
    export SEPARATOR_COLOR="$fg[black]"
    export HOSTNAME_COLOR="$fg[blue]"
    export USERNAME_COLOR="$fg[red]"
    export CURRENT_DIRECTORY_COLOR="$fg[magenta]"
    export SOURCE_CONTROL_COLOR="$fg[white]"
}

function prompt_no_colors {
    export SEPARATOR_COLOR=""
    export HOSTNAME_COLOR=""
    export USERNAME_COLOR=""
    export CURRENT_DIRECTORY_COLOR=""
    export SOURCE_CONTROL_COLOR=""
    export reset_color=""
}

function prompt_basic_colors_with_grey_separator {
    prompt_basic_colors
    export SEPARATOR_COLOR="$FG[239]"
}

function print_with_color {
    echo "%{$2%}$1%{$reset_color%}"
}

function separator {
    print_with_color "$1" "$SEPARATOR_COLOR"
}

export JOB_COUNT_COLOR="$fg[blue]"

prompt_tomorrow_colors
# For reasons which are currently beyond me, it is not possible to use
# $? in PROMPT which is why the second line is so strangely
# constructed.
function set_my_prompt {
    export PROMPT='⚡ % $(print_with_color "%n" "$USERNAME_COLOR") $(separator "at") $(print_with_color "`hostname -s`" "$HOSTNAME_COLOR") $(separator "in") $(print_with_color "`current_directory`" "$CURRENT_DIRECTORY_COLOR")$(git_prompt_info)
$(colored_job_count)%(?.$(print_with_color "$(command_line_character) ❯" $PROMPT_CHAR_SUCCESS).$(print_with_color "$(command_line_character) ❯" $PROMPT_CHAR_ERROR)) '
    export PS2='(%_) '
}

function set_powerline_prompt {
    source "$(python_module_path powerline)/bindings/zsh/powerline.zsh"
}
