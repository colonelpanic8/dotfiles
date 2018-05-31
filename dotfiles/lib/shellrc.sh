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

# TODO(imalison): These need to run every time because of how their
# version managers work. This could cause problems with the situation
# where we want to intentionally override the python/ruby/node
# versions in use in a given shell.

# TODO: Ruby and node are disabled to speed up shell startup...
# See https://github.com/creationix/nvm/issues/709 for nvm
# _node_setup

# XXX: these were moved to _setup_env
# _ruby_setup
# _python_setup
# _node_setup
# _ruby_setup

# travis completion
# XXX: Disabled to reduce performance impact
# [ -f "$HOME/.travis/travis.sh" ] && source "$HOME/.travis/travis.sh"

