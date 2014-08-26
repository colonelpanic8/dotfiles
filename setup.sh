#!/usr/bin/env bash
cd `dirname $BASH_SOURCE` && source resources/bootstrapping.sh
source dotfiles/lib/shellrc/functions.sh
source dotfiles/lib/shellrc/brew.sh
source dotfiles/lib/shellrc/python.sh
source dotfiles/lib/shellrc/vim.sh
DOTFILES_DIRECTORY="$(dotfiles_abspath)/dotfiles"


function symlink_dotfiles() {
    local overwrite=''
    OPTIND=1
    while getopts "o" OPTCHAR;
    do
        case $OPTCHAR in
            o)
                overwrite='yes'
                ;;
        esac
    done
    cd $DOTFILES_DIRECTORY
    [ -a ~/.dotfiles-backups ] && mv ~/.dotfiles-backups ~/.dotfiles-backups.old
    mkdir ~/.dotfiles-backups
    for filename in *; do
        local link_destination="$HOME/.$filename"
        local link_target=$(${readlink_command} -f $filename)
        echo "linking $link_destination to $link_target"
        # Using only test -e doesn't work here because it will return
        # false if the destination of the symbolic link at does not exist.
        test -e $link_destination || test -L $link_destination && test $overwrite && mv $link_destination ~/.dotfiles-backups && ln -si $link_target $link_destination
    done
    [ -a ~/.dotfiles-backups.old ] && mv ~/.dotfiles-backups.old ~/.dotfiles-backups/.dotfiles-backups
}


function symlink_dotfiles_prompt() {
    read -p "Symlinking files from $DOTFILES_DIRECTORY. This ? (y/n) " -n 1
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo
	symlink_dotfiles
    fi
}

function apt-get() {
    INSTALL="sudo apt-get -y install"
    $INSTALL zsh
    $INSTALL tmux
    $INSTALL emacs24-nox
    $INSTALL nmap
    $INSTALL python2.7
    $INSTALL python-pip python-dev
}

function setup_help() {
    echo "setup Usage:
-a Install apt-get packages.
-o Run OSX configuration commands.
-s Symlink dotfiles to home directory.
-b Install brew packages.
-p Install python packages.
-v Setup vim.
-e Do absolutely everything with the most aggresive options.
-h display this help message."
}

function setup() {
    if [[ $# -eq 0 ]] ; then
        setup_help
        exit 0
    fi
    while getopts "aosbpev" OPTCHAR;
    do
        case $OPTCHAR in
            a)
                source resources/apt-get.sh
                ;;
            b)
                do_the_brew_help
                read -p "Enter flags for brew package installation:
"
                [[ $REPLY[0] != '-' ]] && REPLY="-$REPLY"
                do_the_brew $REPLY
                ;;
            o)
                sudo -v
                source resources/osx.sh
                ;;
            s)
                symlink_dotfiles_prompt
                ;;
            p)
                install_python_packages -h
                read -p "Enter flags for python package installation"
                if [[ $REPLY[0] != '-' ]]; then
                    REPLY="-$REPLY"
                fi
                install_python_packages $REPLY
                ;;
            e)
                case uname in
                    Darwin)
                        do_the_brew -au
                        ;;
                    Linux)
                        apt-get
                        ;;
                esac
                install_python_packages -a
                symlink_dotfiles
                source resources/osx.sh
                vimstall
                ;;
            v)
                vimstall
                ;;
            h)
                setup_help
                return
                ;;
        esac
    done
}

setup $@
