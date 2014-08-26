#!/usr/bin/env bash
cd `dirname $BASH_SOURCE` && source resources/bootstrapping.sh
source dotfiles/lib/shellrc/functions.sh
source dotfiles/lib/shellrc/brew.sh
source dotfiles/lib/shellrc/python.sh
DOTFILES_DIRECTORY="$(dotfiles_abspath)/dotfiles"


function symlink_dotfiles() {
    cd $DOTFILES_DIRECTORY
    [ -a ~/.dotfiles-backups ] && mv ~/.dotfiles-backups ~/.dotfiles-backups.old
    mkdir ~/.dotfiles-backups
    for filename in *; do
        local link_destination="$HOME/.$filename"
        local link_target=$(${readlink_command} -f $filename)
        echo "linking $link_destination to $link_target"
        # Using only test -e doesn't work here because it will return
        # false if the destination of the symbolic link at does not exist.
        test -e $link_destination || test -L $link_destination && mv $link_destination ~/.dotfiles-backups
        ln -si $link_target $link_destination
    done
    [ -a ~/.dotfiles-backups.old ] && mv ~/.dotfiles-backups.old ~/.dotfiles-backups/.dotfiles-backups
}


function symlink_dotfiles_prompt() {
    read -p "Symlinking files from $DOTFILES_DIRECTORY. This may overwrite existing files in your home directory. Do you wish to proceed? (y/n) " -n 1
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo
	symlink_dotfiles
    fi
}

function setup_help() {
    echo "setup Usage:
-a Install apt-get packages.
-o Run OSX configuration commands.
-s Symlink dotfiles to home directory.
-b Install brew packages.
-p Install python packages.
-h display this help message."
}

function setup() {
    if [[ $# -eq 0 ]] ; then
        setup_help
        exit 0
    fi
    while getopts "aosbp" OPTCHAR;
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
            h)
                setup_help
                return
                ;;
        esac
    done
}

setup $@
