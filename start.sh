#!/bin/bash
function fedora() {
    yum install make automake gcc gcc-c++ kernel-devel
    yum install 
    yum install python-pip
}

function debian() {
    local INSTALL_COMMAND='sudo apt-get -y install'
    hash apt-get &>/dev/null || (echo 'apt-get is missing.' && exit)
    $INSTALL_COMMAND build-essential git python python-dev python-pip
}

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
    sudo pip install cider
}

function go() {
    git clone https://github.com/IvanMalison/dotfiles.git --recursive
    cd dotfiles
    sudo pip install invoke dotfiles
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
