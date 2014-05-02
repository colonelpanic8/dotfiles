#!/usr/bin/env bash
cd `dirname $BASH_SOURCE` && source resources/bootstrapping.sh
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


function parse_options() {
    while getopts "f" OPTCHAR; do
        case $optchar in
            f)
                symlink_dotfiles
                return
                ;;
        esac
    done
    shift $((OPTIND-1))

    read -p "Symlinking files from $DOTFILES_DIRECTORY. This may overwrite existing files in your home directory. Do you wish to proceed? (y/n) " -n 1
    if [[ $REPLY =~ ^[Yy]$ ]]; then
	symlink_dotfiles
    fi
}

parse_options
