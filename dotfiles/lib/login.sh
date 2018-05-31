source "$HOME/.lib/shellpath.sh"

# XXX: This is duplicated from env with a different var to avoid issues with /etc/profile
command_exists python && setup_unless_environment_variable_exists LOGIN_PATH_SETUP_DONE _setup_path

source_directory_files "$HOME/.lib/login"
