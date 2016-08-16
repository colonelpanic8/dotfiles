source "$HOME/.lib/setup_functions.sh"

function _setup_env {
    _path_helper

    add_to_path "$HOME/.local/bin" "$HOME/.lib/bin" "$HOME/bin" "/usr/local/bin" --before
    _ruby_setup
    _python_setup
    is_osx && _osx_path_setup
    _java_setup
    _go_setup
    _rust_setup
    _tex_setup

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
        eval "$(pyenv init - --no-rehash)"
    else
        echo "WARNING: pyenv is not installed on this machine and python will likely not function correctly"
    fi

    # if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi

    add_to_path "$HOME/.lib/python" --path-var 'PYTHONPATH'
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
    export RBENV_ROOT="$(brew --prefix rbenv)"
    add_to_path "$RBENV_ROOT/bin"
    hash rbenv 2> /dev/null && eval "$(rbenv init - --no-rehash)"
}

function _tex_setup {
    is_osx && add_to_path "/Library/TeX/texbin/"
}

function _path_helper {
    if [ -f /usr/libexec/path_helper ];
    then
        eval "$(/usr/libexec/path_helper -s)"
    fi
}
