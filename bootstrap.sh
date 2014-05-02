#!/usr/bin/env bash
cd `dirname $BASH_SOURCE` && source resources/bootstrapping.sh
DOTFILES_DIRECTORY="$(dotfiles_abspath)/dotfiles"

function make_powerline_symlinks() {
    # Make a powerline link if powerline is installed
    local powerline_location=$(pip show Powerline | grep Location | awk '{print $2}')
    local conf_location="/powerline/bindings/tmux/powerline.conf"
    local link_destination="$HOME/.tmux.powerline"
    if test -z $powerline_location;
    then
        sudo pip install --user git+git://github.com/Lokaltog/powerline
    fi

    if test -z $powerline_location;
    then
        rm $link_destination 2> /dev/null
        touch "$link_destination"
    else
        ln -si "$powerline_location$conf_location" $link_destination
    fi
}

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
    make_powerline_symlinks
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
