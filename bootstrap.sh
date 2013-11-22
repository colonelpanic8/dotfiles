#!/bin/bash
case `uname` in
    'Darwin')
        readlink_command='greadlink'
	;;
    *)
        readlink_command='readlink'
esac

CURRENT_DIRECTORY="$(dirname "${BASH_SOURCE}" | xargs "${readlink_command}" -f)/dotfiles"
cd $CURRENT_DIRECTORY

echo "Linking From $CURRENT_DIRECTORY"

function symlink_dotfiles() {
    [[ -a ~/.dotfiles-backups ]] || mkdir ~/.dotfiles-backups
    for filename in *; do
        local link_destination="$HOME/.$filename"
        local absolute_path="$($readlink_command -f $filename)"
        ln -si $absolute_path $link_destination
    done
}

if [ "$1" == "--force" -o "$1" == "-f" ]; then
    doIt
else
    read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
	symlink_dotfiles
    fi
fi
unset symlink_dotfiles
