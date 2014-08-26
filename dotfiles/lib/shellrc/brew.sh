function brew_install_items() {
    for package_install_string in $@;
    do
        package_args=$package_install_string
        if is_zsh;
            then
            # Horrible hack to induce word splitting.
            eval "package_args=($package_install_string)"
        fi
        brew install $package_args;
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
        "emacs" \
        "tmux" \
        "python --with-brewed-openssl" \
        "htop" \
        "greadlink" \
        "zsh" \
        "make" \
        "macvim --override-system-vim --custom-system-icons --with-features=huge --enable-rubyinterp --enable-pythoninterp --enable-perlinterp --enable-cscope" \
    )
    # `find`, `locate`, `updatedb`, and `xargs`, g-prefixed
    # core utilities (those that come with OS X are outdated)
    BASICS=(
        "findutils" \
        "coreutils" \
        "binutils" \
        "diffutils" \
        "ed --default-names" \
        "gawk" \
        "gnu-indent --default-names" \
        "gnu-sed --default-names" \
        "gnu-tar --default-names" \
        "gnu-which --default-names" \
        "gnutls --default-names" \
        "grep --default-names" \
        "gzip" \
        "watch" \
        "wdiff --with-gettext" \
        "wget --enable-iri" \
    )
    SHOULD_INSTALL=(
        "nmap" \
        "readline" \
        "netcat" \
        "reattach-to-user-namespace" \
        "daemonize" \
        "ngrep" \
        "gist" \
        "gawk" \
        "pstree" \
        "ack" \
        "hub" \
        "tig" \
        "heroku" \
        "scala" \
        "sbt" \
        "node" \
        "npm" \
    )
    MISC=("file-formula" \
        "less" \
        "openssh --with-brewed-openssl" \
        "perl518" \
        "rsync" \
        "svn" \
        "unzip" \
    )

    install_items=()
    OPTIND=1
    while getopts "uebsmah" OPTCHAR;
    do
        case $OPTCHAR in
            u)
                brew upgrade
                ;;
            e)
                install_items=("${install_items[@]}" "${ESSENTIAL[@]}")
                fix_brew_htop
                ;;
            b)
                install_items=("${install_items[@]}" "${BASICS[@]}")
                ;;
            s)
                install_items=("${install_items[@]}" "${SHOULD_INSTALL[@]}")
                ;;
            m)
                install_items=("${install_items[@]}" "${MISC[@]}")
                ;;
            a)
                install_items=("${install_items[@]}" "${ESSENTIAL[@]}" "${BASICS[@]}" "${SHOULD_INSTALL[@]}" "${MISC[@]}")
                ;;
            h)
                do_the_brew_help
                return
                ;;
        esac
    done
    echo "Installing the following packages:"
    for package_name in "${install_items[@]}"; do echo $package_name; done;
    brew update
    brew_install_items $install_items
    brew cleanup
}

function do_the_brew_help() {
    echo "do_the_brew Usage:
-a install all packages.
-u upgrade brew packages.
-e install essential packages
-b install GNU basics including replacements for non GNU versions included in OSX.
-s install a wider range of packages.
-m install very non-essential packages.
-h display this help message.
"
}
