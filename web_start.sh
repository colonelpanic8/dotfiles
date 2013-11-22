#!/bin/bash
function debian() {
    hash apt-get &>/dev/null || (echo 'apt-get is missing.' && exit)
    sudo apt-get -y install build-essential
    sudo apt-get -y install git
}

function osx() {
    hash gcc &>/dev/null
    if [ $? -ne 0 ]
    then
	echo "gcc not found."
	exit
    fi
    hash brew &>/dev/null && echo "brew found" || ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
    brew update
    brew install git
}

case `uname` in
    'Darwin')
	osx
	;;
    'Linux')
	debian
        ;;
esac

git clone git@github.com:IvanMalison/dotfiles.git
cd dotfiles
