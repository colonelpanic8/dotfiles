#!/usr/bin/env bash


function brew_for_multiple_users() {
    sudo chgrp -R admin /usr/local
    sudo chmod -R g+w /usr/local
    sudo chgrp -R admin /Library/Caches/Homebrew
    sudo chmod -R g+w /Library/Caches/Homebrew
}

function osx() {
    xcode-select --install
    hash brew &>/dev/null && echo "brew found" || ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
    brew_for_multiple_users
    brew doctor
    brew update
    brew install git
    brew install python
    sudo easy_install pip
}

function go() {
    git clone https://github.com/IvanMalison/dotfiles.git --recursive
    cd dotfiles
    sudo pip install invoke
    invoke setup
}

while true; do sudo -n true; sleep 60; kill -0 "$$" || exit; done 2>/dev/null &
case `uname` in
    'Darwin')
        osx
        ;;
    'Linux')
        debian
        ;;
esac

go
