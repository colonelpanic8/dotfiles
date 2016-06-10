. "$HOME/.lib/shellenv/functions.sh"
command -v greadlink > /dev/null && alias readlink="greadlink"

function _source_shellenv_files {
    for filename in ~/.lib/shellenv/*; do
        source $filename
    done
}

function add_to_path {
    local result
    result=$($HOME/.lib/python/shell_path.py --include-assignment "$@")
    eval "$result"
}

function get_python_scripts_path {
    python -c "import sysconfig; print sysconfig.get_path('scripts')"
}

function _setup_env {
    _path_helper
    add_to_path /usr/local/lib/python2.7/site-packages --after
    add_to_path "$HOME/.rvm/bin" --after
    add_to_path "$HOME/bin"
    hash brew 2>/dev/null && add_to_path --before "$(brew --prefix coreutils)/libexec/gnubin"
    add_to_path "/usr/local/bin"

    add_to_path "$(get_python_scripts_path)" --before

    if is_osx; then
        export CFLAGS=-Qunused-arguments
        export CPPFLAGS=-Qunused-arguments
        add_to_path "/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources" --after
        local JDK_LOCATION="$(find /Library/Java/JavaVirtualMachines -depth 1 | head -n 1)"
        export JAVA_HOME="$JDK_LOCATION/Contents/Home"
        export STUDIO_JDK=$JDK_LOCATION
        export GRADLE_HOME="$(brew --prefix gradle)"
        export ANDROID_HOME="$(brew --prefix android-sdk)"
        add_to_path "$ANDROID_HOME" --after
        
        # Access gnu man pages.
        hash brew 2> /dev/null && export MANPATH="$(brew --prefix)/opt/coreutils/libexec/gnuman:$MANPATH"
    else 
        export JAVA_HOME="$(update-alternatives --config java | get_cols ' -1' | head -n 1)"
        is_osx && export VISUAL="which emacsclient -c -n"
    fi

    add_to_path "$JAVA_HOME/bin"

    add_to_path "$HOME/.lib/python" --after
    add_to_path "/usr/local/sbin" --after
    add_to_path "$HOME/.cargo/bin"

    # Load RVM into a shell session *as a function*
    [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

    function with_shellrc {
        zsh -c "source ~/.zshrc && ""$@"
    }

    # Travis completion
    [ -f "$HOME/.travis/travis.sh" ] && source "$HOME/.travis/travis.sh"

    export NVM_DIR="/Users/imalison/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
    export NODE_PATH="/usr/local/lib/node_modules/"

    add_to_path "$HOME/.local/bin"
    add_to_path "$HOME/.lib/python" --path-var 'PYTHONPATH'

    add_to_path "$HOME/go" --path-var 'GOPATH'
    add_to_path "${GOPATH//://bin:}/bin"

    export RBENV_ROOT=/usr/local/var/rbenv
    add_to_path "$HOME/.rbenv/bin"
    hash rbenv 2> /dev/null && eval "$(rbenv init -)"
    hash brew 2>/dev/null && add_to_path "$(brew --prefix coreutils)/libexec/gnubin"

    add_to_path "$HOME/.lib/bin"
    export ENVIRONMENT_SETUP_DONE="$(date)"
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
_source_shellenv_files
