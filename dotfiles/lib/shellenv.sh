. "$HOME/.lib/shellenv/functions.sh"
command -v greadlink > /dev/null && alias readlink="greadlink"

function _source_shellenv_files {
    for filename in ~/.lib/shellenv/*; do
        source $filename
    done
}

function add_to_path {
    local python_command
    local result

    # We need to get a path to the ACTUAL python command because
    # pyenv alters PATH before actually executing python, which ends
    # up changing PATH in a way that is not desireable.
    hash pyenv && python_command="$(pyenv which python)" || python_command="$(which python)"

    result=$($python_command $HOME/.lib/python/shell_path.py --include-assignment "$@")
    eval "$result"
}

function get_python_scripts_path {
    python -c "import sysconfig; print sysconfig.get_path('scripts')"
}

function _setup_env {
    _path_helper

    time add_to_path "$HOME/.local/bin" "$HOME/.lib/bin" "$HOME/bin" "/usr/local/bin" --before
    is_osx && _osx_path_setup
    _java_setup
    _go_setup
    _rust_setup
    _tex_setup

    # Travis completion
    # [ -f "$HOME/.travis/travis.sh" ] && source "$HOME/.travis/travis.sh"

    export ENVIRONMENT_SETUP_DONE="$(date)"
}

function _osx_path_setup {
    hash brew 2>/dev/null && add_to_path --before "$(brew --prefix coreutils)/libexec/gnubin"

    # Adds airport utility
    add_to_path "/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources" --after

    # I believe that this is here because of some issue with
    # python builds in OSX
    export CFLAGS=-Qunused-arguments
    export CPPFLAGS=-Qunused-arguments
}

function _python_setup {
    add_to_path "$HOME/.lib/python" --after
    export PYENV_ROOT="/usr/local/var/pyenv"

    if which pyenv > /dev/null; then
        local PYENV_INIT_COMMANDS="$(pyenv init -)"
        eval "$PYENV_INIT_COMMANDS"
    else
        echo "WARNING: pyenv not is installed on this machine and python will likely not function correctly"
    fi

    # The following line is no longer necessary since the pyenv shim
    # should handle directing to the appropriate install.

    # add_to_path "$(get_python_scripts_path)" --before

    add_to_path "$HOME/.lib/python" --path-var 'PYTHONPATH'

    # This should no longer be needed now that pyenv is used
    # add_to_path /usr/local/lib/python2.7/site-packages --after
}

function _node_setup {
    # node/nvm
    export NVM_DIR="/Users/imalison/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
    export NODE_PATH="/usr/local/lib/node_modules/"
}

function _java_setup {
    if is_osx; then
        local JDK_LOCATION="$(find /Library/Java/JavaVirtualMachines -depth 1 | head -n 1)"
        export JAVA_HOME="$JDK_LOCATION/Contents/Home"
        export STUDIO_JDK=$JDK_LOCATION
        export GRADLE_HOME="$(brew --prefix gradle)"
        export ANDROID_HOME="$(brew --prefix android-sdk)"
        add_to_path "$ANDROID_HOME" --after
        
        # Access gnu man pages.
        hash brew 2> /dev/null && export MANPATH="$(brew --prefix)/opt/coreutils/libexec/gnuman:$MANPATH"
    else
        # This may be ubuntu/debian specific
        export JAVA_HOME="$(update-alternatives --config java | get_cols ' -1' | head -n 1)"
        is_osx && export VISUAL="which emacsclient -c -n"
    fi

    add_to_path "$JAVA_HOME/bin"
}

function _go_setup {
    add_to_path "$HOME/go" --path-var 'GOPATH'
    add_to_path "${GOPATH//://bin:}/bin"
}

function _rust_setup {
    add_to_path "$HOME/.cargo/bin"
}

function _ruby_setup {
    # Load RVM into a shell session *as a function*
    [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"
    export RBENV_ROOT=/usr/local/var/rbenv
    add_to_path "$HOME/.rbenv/bin"
    hash rbenv 2> /dev/null && eval "$(rbenv init -)"
}

function _tex_setup {
    is_osx && add_to_path "/Library/TeX/texbin/"
}

function _path_helper {
    if [ -f /usr/libexec/path_helper ];
    then
        export PATH_HELPER_RAN="$(date)"
        eval "$(/usr/libexec/path_helper -s)"
    fi
}

environment_variable_exists PATH_HELPER_RAN || _path_helper
environment_variable_exists ENVIRONMENT_SETUP_DONE || _setup_env

# TODO(imalison): These need to run every time because of how their
# version managers work. This could cause problems with the situation
# where we want to intentionally override the python/ruby/node
# versions in use in a given shell.
_node_setup
_ruby_setup
_python_setup

_source_shellenv_files
