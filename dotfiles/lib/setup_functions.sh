function command_exists {
    hash "$1" 2>/dev/null 1>/dev/null
}

function shell_contains () {
  local e
  for e in "${@:2}"; do
      [[  "$1" == *"$e"* ]] && return 0
  done
  return 1
}

function _set_python_command {
    # See comment in add_to_path about why this is necessary
    if command_exists pyenv;
    then
        _python_command="$(pyenv which python)"
    else
        which pyenv
        _python_command="$(which python)"
    fi
    shell_contains "$_python_command" "shim" && \
        echo "Warning: setting python command to shim"
}

function add_to_path {
    environment_variable_exists _python_command || _set_python_command
    # We need to get a path to the ACTUAL python command because
    # pyenv alters PATH before actually executing python, which ends
    # up changing PATH in a way that is not desireable.
    eval "$($_python_command $HOME/.lib/python/shell_path.py --include-assignment "$@")"
}

# Taken from http://www.unix.com/shell-programming-and-scripting/27932-how-know-linux-distribution-i-am-using.html
function get_distro {
    # start with uname and branch the decision from there
    dist=$(uname -s 2> /dev/null)
    if [ "$dist" = "Linux" ]; then
        get_linux_distro && return 0
    elif [ -n "$dist" ]; then
        echo "$dist"
        return 0
    fi
    proc_version || echo "Unknown"
    return 1
}

function get_linux_distro {
   if [ -r /etc/lsb-release ]; then
       dist=$(grep 'DISTRIB_ID' /etc/lsb-release | sed 's/DISTRIB_ID=//' | head -1)
       [ -n "$dist" ] && echo "$dist" && return 0
   fi

   dist=$(find /etc/ -maxdepth 1 -name '*release' 2> /dev/null | sed 's/\/etc\///' | sed 's/-release//' | head -1)
   [ -n "$dist" ] && echo "$dist" && return 0

   dist=$(find /etc/ -maxdepth 1 -name '*version' 2> /dev/null | sed 's/\/etc\///' | sed 's/-version//' | head -1)
   [ -n "$dist" ] && echo "$dist" && return 0
   return 1
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

function source_directory_files {
	for filename in "$1"/*; do
		environment_variable_exists SHELL_STARTUP_DEBUG && echo "Sourcing $filename"
        source "$filename"
    done
}
