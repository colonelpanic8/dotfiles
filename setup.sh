#!/usr/bin/env bash
function setup() {
    cd `dirname $BASH_SOURCE` && source resources/bootstrapping.sh
    source dotfiles/lib/shellrc/functions.sh
    source dotfiles/lib/shellrc/brew.sh
    source dotfiles/lib/shellrc/python.sh

    case `uname` in
        'Darwin')          
            osx_setup
            ;;
        'Linux')
            source resources/apt-get.sh
            ;;
    esac

    install_powerline
    source bootstrap.sh
}


function osx_setup() {
    while getopts "uebsmaho" OPTCHAR;
    do
        case $OPTCHAR in
            h)
                echo "brew options:"
                do_the_brew -h
                exit
                ;;
    source resources/osx.sh
    do_the_brew $@
}
