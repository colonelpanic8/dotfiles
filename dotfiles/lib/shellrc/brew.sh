function brew_install_items() {
    for package_install_string in $@;
    do
        # Horrible hack to induce word splitting.
        eval "package_args=($package_install_string)"
        brew install $package_args;
        brew link $package_args[0];
    done
}

# htop wont display all process information if the owner is not root
function fix_brew_htop() {
    sudo chmod 777 $(readlink -f `which htop`)
    sudo chown root $(readlink -f `which htop`)
    sudo chmod 6555 `which htop`
}

function do_the_brew() {
    ESSENTIAL=(
        "emacs"
        "git"
        "tmux"
        "python --with-brewed-openssl"
        "htop"
        "greadlink"
        "vim --override-system-vi"
        "zsh"
    )
    # `find`, `locate`, `updatedb`, and `xargs`, g-prefixed
    # core utilities (those that come with OS X are outdated)
    BASICS=(
        "findutils"
        "coreutils"
        "binutils"
        "diffutils"
        "ed --default-names"
        "gawk"
        "gnu-indent --default-names"
        "gnu-sed --default-names"
        "gnu-tar --default-names"
        "gnu-which --default-names"
        "gnutls --default-names"
        "grep --default-names"
        "gzip"
        "watch"
        "wdiff --with-gettext"
        "wget --enable-iri"
    )
    SHOULD_INSTALL=(
        "watch"
        "nmap"
        "readline"
        "netcat"
        "reattach-to-user-namespace"
        "daemonize"
        "ngrep"
        "gist"
        "gawk"
        "pstree"
        "ack"
        "hub"
        "tig"
        "heroku"
        "make"
    )
    MISC=(
        "file-formula"
        "git"
        "less"
        "openssh --with-brewed-openssl"
        "perl518"
        "python --with-brewed-openssl"
        "rsync"
        "svn"
        "unzip"
        "macvim --override-system-vim --custom-system-icons"
    )
    SCALA=(scala sbt)
    JAVASCRIPT=(node npm)

    brew update
    brew tap homebrew/dupes
    brew install homebrew/dupes/grep
    while getopts "uebsma" OPTCHAR;
    do
        case $OPTCHAR in
            u)
                brew upgrade
                ;;
            e)
                brew_install_items $ESSENTIAL
                fix_brew_htop
                ;;
            b)
                brew_install_items $BASICS
                ;;
            s)
                brew_install_items $SHOULD_INSTALL
                ;;
            m)
                brew_install_items $MISC
                brew_install_items $SCALA
                brew_install_items $JAVASCRIPT
                ;;
            a)
                brew_install_items $ESSENTIAL
                brew_install_items $BASICS
                brew_install_items $SHOULD_INSTALL
                brew_install_items $MISC
                brew_install_items $SCALA
                brew_install_items $JAVASCRIPT
                ;;
        esac
    done
    brew cleanup
}
