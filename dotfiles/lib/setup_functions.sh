_python_command=""

function _set_python_command {
    # See comment in add_to_path about why this is necessary
    if hash pyenv 2>/dev/null;
    then
        _python_command="$(pyenv which python)"
    else
        _python_command="$(which python)"
    fi
}

_set_python_command

function add_to_path {
    # We need to get a path to the ACTUAL python command because
    # pyenv alters PATH before actually executing python, which ends
    # up changing PATH in a way that is not desireable.
    eval "$($_python_command $HOME/.lib/python/shell_path.py --include-assignment "$@")"
}

function is_osx() {
    case `uname` in
        'Darwin')
            return 0
	    ;;
        *)
            return 1;
            ;;
    esac
}

function environment_variable_exists {
    eval "value=\"\${$1+x}\""
    [ ! -z $value ]
}

function command_exists {
    hash "$1" 2>/dev/null 1>/dev/null
}
