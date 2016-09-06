source_directory_files "$HOME/.lib/shellrc"

[ -s "/usr/local/bin/virtualenvwrapper.sh" ] && . /usr/local/bin/virtualenvwrapper.sh
function j() {
    (( $+commands[brew] )) && {
        local pfx=$(brew --prefix autojump)
        [[ -f "$pfx/etc/autojump.sh" ]] && . "$pfx/etc/autojump.sh"
        j "$@"
    }
}
environment_variable_exists INSIDE_EMACS && inside_emacs_hook

# XXX: This used to be in shellenv and should be moved back once these
# are faster.
_node_setup
_ruby_setup

# travis completion
# XXX: Disabled to reduce performance impact
# [ -f "$HOME/.travis/travis.sh" ] && source "$HOME/.travis/travis.sh"
