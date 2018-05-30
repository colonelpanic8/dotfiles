source "$HOME/.lib/shellpath.sh"

setup_unless_environment_variable_exists ENV_PATH_SETUP_DONE || _setup_path

source_directory_files "$HOME/.lib/login"
