#!/bin/sh
case `uname` in
    'Darwin')
        readlink_command='greadlink'
	;;
    *)
        readlink_command='readlink'
esac
DOTFILES_DIRECTORY="$(dirname "${BASH_SOURCE}" | xargs "${readlink_command}" -f)/dotfiles"

function symlink_dotfiles() {
    cd $DOTFILES_DIRECTORY
    [[ -a ~/.dotfiles-backups ]] && mv ~/.dotfiles-backups ~/.dotfiles-backups.old
    mkdir ~/.dotfiles-backups
    for filename in *; do
        local link_destination="$HOME/.$filename"
        local absolute_path="$(${readlink_command} -f $filename)"
        echo "linking $link_destination to $absolute_path"
        [[ -a $link_destination ]] && mv $link_destination ~/.dotfiles-backups
        ln -si $absolute_path $link_destination
    done
    [[ -a ~/.dotfiles-backups ]] && mv ~/.dotfiles-backups.old ~/.dotfiles-backups/.dotfiles-backups
}

if [ "$1" == "--force" -o "$1" == "-f" ]; then
    symlink_dotfiles
else
    read -p "Symlinking files from $DOTFILES_DIRECTORY. This may overwrite existing files in your home directory. Do you wish to proceed? (y/n) " -n 1
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
	symlink_dotfiles
    fi
fi

unset symlink_dotfiles
unset DOTFILES_DIRECTORY
