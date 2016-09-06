source "$HOME/.lib/shellpath.sh"

environment_variable_exists ENVIRONMENT_SETUP_DONE || _setup_env

# TODO(imalison): These need to run every time because of how their
# version managers work. This could cause problems with the situation
# where we want to intentionally override the python/ruby/node
# versions in use in a given shell.

# TODO: Ruby and node are disabled to speed up shell startup...
# See https://github.com/creationix/nvm/issues/709 for nvm
# _node_setup

# XXX: these were moved to _setup_env
# _ruby_setup
_python_setup

source_directory_files "$HOME/.lib/shellenv"
