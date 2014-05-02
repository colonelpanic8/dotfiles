readlink_command='readlink'
command -v greadlink > /dev/null && readlink_command="greadlink"
function dotfiles_abspath() {
    echo "$(${readlink_command} -f "$BASH_SOURCE" | xargs dirname | xargs dirname)"
}
