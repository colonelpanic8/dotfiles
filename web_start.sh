#!/bin/bash
function debian() {
    hash apt-get &>/dev/null || (echo 'apt-get is missing.' && exit)
    sudo apt-get -y install build-essential
    sudo apt-get -y install git
}

function brew_for_multiple_users() {
    sudo chgrp -R admin /usr/local
    sudo chmod -R g+w /usr/local
    sudo chgrp -R admin /Library/Caches/Homebrew
    sudo chmod -R g+w /Library/Caches/Homebrew
}

function osx() {
    hash gcc &>/dev/null
    if [ $? -ne 0 ]
    then
	echo "gcc not found."
	exit
    fi
    hash brew &>/dev/null && echo "brew found" || ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
    brew_for_multiple_users
    brew update
    brew install git
}

function go() {
    git clone git@github.com:IvanMalison/dotfiles.git
    cd dotfiles
    ./setup.sh -e
}

case `uname` in
    'Darwin')
	osx
        go
	;;
    'Linux')
	debian
        go
        ;;
esac
