source ~/.lib/shellenv/functions.sh
command -v greadlink > /dev/null && alias readlink="greadlink"

function _source_shellenv_files {
    for filename in ~/.lib/shellenv/*; do
        source $filename
    done
}

function _setup_env {
    _path_helper
    idem_add_to_back_of_path "$HOME/.local/lib/python2.6/site-packages"
    idem_add_to_back_of_path "$HOME/.rvm/bin"
    idem_add_to_front_of_path "$HOME/bin"
    hash brew 2>/dev/null && idem_add_to_front_of_path "$(brew --prefix coreutils)/libexec/gnubin"
    idem_add_to_front_of_path "/usr/local/bin"

    idem_add_to_back_of_path `python -c 'import sysconfig; print sysconfig.get_path("scripts")'`

    if is_osx; then
        export CFLAGS=-Qunused-arguments
        export CPPFLAGS=-Qunused-arguments
        idem_add_to_back_of_path "/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources"
        local JDK_LOCATION="$(find /Library/Java/JavaVirtualMachines -depth 1 | head -n 1)"
        export JAVA_HOME="$JDK_LOCATION/Contents/Home"
        export STUDIO_JDK=$JDK_LOCATION
        export GRADLE_HOME="$(brew --prefix gradle)"
        export ANDROID_HOME="$(brew --prefix android-sdk)"
        idem_add_to_back_of_path "$ANDROID_HOME"
        
        # Access gnu man pages.
        hash brew 2> /dev/null && export MANPATH="$(brew --prefix)/opt/coreutils/libexec/gnuman:$MANPATH"
    else 
        export JAVA_HOME="$(update-alternatives --config java | get_cols ' -1' | head -n 1)"
        is_osx && export VISUAL="which emacsclient -c -n"
    fi

    idem_add_to_front_of_path "$JAVA_HOME/bin"

    idem_add_to_back_of_path "$HOME/.lib/python"
    idem_add_to_back_of_path "/usr/local/sbin"

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

    idem_add_to_front_of_path "$HOME/.local/bin"
    idem_add_to_front_of_path "$HOME/.lib/python" 'PYTHONPATH'

    idem_add_to_front_of_path "$HOME/go" 'GOPATH'
    idem_add_to_front_of_path "${GOPATH//://bin:}/bin"

    export RBENV_ROOT=/usr/local/var/rbenv
    idem_add_to_front_of_path "$HOME/.rbenv/bin"
    hash rbenv 2> /dev/null && eval "$(rbenv init -)"
    hash brew 2>/dev/null && idem_add_to_front_of_path "$(brew --prefix coreutils)/libexec/gnubin"

    idem_add_to_front_of_path "$HOME/.lib/bin"
    export ENVIRONMENT_SETUP_DONE="$(date)"
}

function _path_helper {
    export PATH_HELPER_RAN="$(date)"
    eval `/usr/libexec/path_helper -s`
}

environment_variable_exists PATH_HELPER_RAN || _path_helper
environment_variable_exists ENVIRONMENT_SETUP_DONE || _setup_env
_source_shellenv_files
