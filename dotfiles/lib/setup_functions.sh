function command_exists {
    hash "$1" 2>/dev/null 1>/dev/null
}

function run_if_exists {
	command_exists "$@" && "$@"
}

function shell_contains () {
  local e
  for e in "${@:2}"; do
      [[  "$1" == *"$e"* ]] && return 0
  done
  return 1
}

function environment_variable_exists {
    eval "value=\"\${$1+x}\""
    [ ! -z $value ]
}

function setup_unless_environment_variable_exists {
	environment_variable_exists "$1" || { $2 && export "$1=$(date)"; }
}

function add_to_path {
    eval "$(python $HOME/.lib/python/shell_path.py --include-assignment "$@")"
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

   if [ -r /etc/os-release ]; then
	   dist=$(grep 'ID=' /etc/os-release | sed 's/ID=//' | head -1)
	   [ -n "$dist" ] && echo "$dist" && return 0
   fi

   dist=$(find /etc/ -maxdepth 1 -name '*release' 2> /dev/null | sed 's/\/etc\///' | sed 's/-release//' | head -1)
   [ -n "$dist" ] && echo "$dist" && return 0

   dist=$(find /etc/ -maxdepth 1 -name '*version' 2> /dev/null | sed 's/\/etc\///' | sed 's/-version//' | head -1)
   [ -n "$dist" ] && echo "$dist" && return 0
   return 1
}


function is_osx() {
	if command_exists uname; then
			case `uname` in
				'Darwin')
					return 0;
					;;
				*)
					return 1;
					;;
			esac
	else
		return 1
	fi
}

function source_directory_files {
	for filename in "$1"/*; do
		environment_variable_exists SHELL_STARTUP_DEBUG && echo "Sourcing $filename"
        source "$filename"
    done
}
