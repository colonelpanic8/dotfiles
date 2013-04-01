#!/bin/bash
cd "$(dirname "${BASH_SOURCE}")"
function doIt() {
    rsync --exclude "setup.sh" --exclude "Monaco-Powerline.otf" --exclude "web_start.sh" \
        --exclude "oh-my-zsh" --exclude "tmux-powerline" --exclude ".git/" \
        --exclude ".DS_Store" --exclude "bootstrap.sh" --exclude "README.md" \
        --exclude "requirements.txt" --exclude "more_python.txt" \
        -av . ~
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
