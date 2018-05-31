source "$HOME/.lib/shellpath.sh"

# XXX: This PATH setup var is only actually set in login.sh which is definitely
# strange. The reason for this is that because env runs BEFORE profile, things
# in places like /etc/profile can actually clobber the path setup that we do.

# Disbled setup here. Doing it in profile instead
if command_exists python; then
	environment_variable_exists LOGIN_PATH_SETUP_DONE || setup_unless_environment_variable_exists ENV_PATH_SETUP_DONE _setup_path
fi

source_directory_files "$HOME/.lib/shellenv"
