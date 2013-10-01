#!/bin/bash
case `uname` in
    'Darwin')
        readlink_command='greadlink'
	;;
    *)
        readlink_command='readlink'
esac

CURRENT_DIRECTORY="$(dirname "${BASH_SOURCE}" | xargs "${readlink_command}" -f)"
cd $CURRENT_DIRECTORY

echo "Linking From $CURRENT_DIRECTORY"

function doIt() {
    [[ -a ~/.dotfiles-backups ]] || mkdir ~/.dotfiles-backups
    exclude_list="setup.sh Monaco-Powerline.otf web_start.sh oh-my-zsh tmux-powerline .git .gitmodules .DS_store bootstrap.sh README.md more_python.txt . .. requirements.txt"

    for i in .*; do
        if ! [ -z ${i/*.swp/} ] && ! [[ $exclude_list =~ $i ]]
        then
            [[ -a ~/$i ]] && mv ~/$i ~/.dotfiles-backups/$i
            ln -si $CURRENT_DIRECTORY/$i ~/$i
        fi
    done
}

if [ "$1" == "--force" -o "$1" == "-f" ]; then
    doIt
else
    read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
	doIt
    fi
fi
unset doIt
source ~/.bash_profile
