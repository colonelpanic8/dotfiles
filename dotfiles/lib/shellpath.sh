source "$HOME/.lib/setup_functions.sh"

function _setup_env {
    _path_helper

    # XXX/TODO:
    # This is in shellenv.sh now
    _python_setup

    add_to_path "$HOME/.local/bin" "$HOME/.lib/bin" "$HOME/bin" --before
    add_to_path "/usr/local/sbin" "/usr/local/bin" "/usr/bin" --after
    _ruby_setup

    is_osx && _osx_path_setup || _linux_path_setup
    _emacs_setup
    _haskell_setup
    _java_setup
    _go_setup
    _racket_setup
    _rust_setup
    _tex_setup

    # This makes systemd aware of change to $PATH
    run_if_exists systemctl --user import-environment PATH DISPLAY XAUTHORITY HOME
    export ENVIRONMENT_SETUP_DONE="$(date)"
}

function _linux_path_setup {
    add_to_path "/usr/lib/gnome-settings-daemon" "/usr/lib/notify-osd-customizable"
}

function _osx_path_setup {
    if command_exists "brew";
    then
       add_to_path --before "$(brew --prefix coreutils)/libexec/gnubin"
       # Access gnu man pages.
       add_to_path "$(brew --prefix)/opt/coreutils/libexec/gnuman" --path-var "MANPATH"
    fi

    # Adds airport utility
    add_to_path "/System/Library/PrivateFrameworks/Apple80211.framework/Versions/Current/Resources" --after

    # I believe that this is here because of some issue with python builds in
    # OSX
    export CFLAGS=-Qunused-arguments
    export CPPFLAGS=-Qunused-arguments
}

function _python_setup {
    export PYENV_ROOT="/usr/local/var/pyenv"

    if which pyenv > /dev/null; then
        eval "$(pyenv init - --no-rehash)"
    else
        echo "WARNING: pyenv is not installed on this machine and python will likely not function correctly"
    fi

    add_to_path "$HOME/.lib/python" --after
    # if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi

    add_to_path "$HOME/.lib/python" --path-var 'PYTHONPATH'
}

function _node_setup {
    # node/nvm
    if [ -e /usr/share/nvm/nvm.sh ]; then
        # This used to be init-nvm.sh but that automatically loads bash
        # completion which can be quite annoying.
        source /usr/share/nvm/nvm.sh
    else
        export NVM_DIR="$HOME/.nvm"
        [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
        export NODE_PATH="/usr/local/lib/node_modules/"
    fi
}

function _java_setup {
    if is_osx; then
        local JDK_LOCATION="$(find /Library/Java/JavaVirtualMachines -depth 1 | head -n 1)"
        export JAVA_HOME="$JDK_LOCATION/Contents/Home"
        export STUDIO_JDK=$JDK_LOCATION
        export GRADLE_HOME="$(brew --prefix gradle)"
        export ANDROID_HOME="$(brew --prefix android-sdk)"
        add_to_path "$ANDROID_HOME" --after
    fi

    case get_distro in
        Arch)
        # Arch does not seem to need to set JAVA_HOME
            ;;
        Debian)
        # This may be ubuntu/debian specific
            export JAVA_HOME="$(update-alternatives --config java | get_cols ' -1' | head -n 1)"
            ;;
    esac

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
    if is_osx;
    then
        export RBENV_ROOT="$(brew --prefix rbenv)"
        add_to_path "$RBENV_ROOT/bin"
    else
        add_to_path "$HOME/.rbenv/shims"
    fi
    hash rbenv 2> /dev/null && eval "$(rbenv init - --no-rehash)"
}

function _tex_setup {
    is_osx && add_to_path "/Library/TeX/texbin/"
}

function _racket_setup {
    if is_osx; then
        local racket_base_path="$(brew --prefix racket)"
        # XXX: Seems maybe this is not needed
        # local newest_version_number="$(ls \"$racket_base_path\" | sort -Vr | head -n1)"
        add_to_path "$racket_base_path/bin" --before
    fi
}

function _emacs_setup {
    add_to_path "$HOME/.evm/bin" "$HOME/.cask/bin" --before
    command_exists evm || curl -fsSkL https://raw.github.com/rejeep/evm/master/go | bash
}

function _path_helper {
    if [ -f /usr/libexec/path_helper ];
    then
        eval "$(/usr/libexec/path_helper -s)"
    fi
}

function _haskell_setup {
    # We put cabal after local/bin because we want stack installs to take
    # precedence.
    add_to_path "$HOME/.cabal/bin" --after --target "$HOME/.local/bin"
}
