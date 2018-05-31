source "$HOME/.lib/shellpath.sh"

command_exists python && setup_unless_environment_variable_exists NIX_PATH_SETUP_DONE _setup_path

