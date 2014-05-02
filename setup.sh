#!/usr/bin/env bash
cd `dirname $BASH_SOURCE` && source resources/bootstrapping.sh
source dotfiles/lib/shellrc/functions.sh


case `uname` in
    'Darwin')
        source resources/osx.sh
        source resources/brew.sh
        ;;
    'Linux')
        source resources/apt-get.sh
        ;;
esac


function install_powerline() {
    hash pip 2>/dev/null || sudo easy_install pip
    if test -z $(pip show Powerline | grep Location | awk '{print $2}');
    then
        sudo pip install --user git+git://github.com/Lokaltog/powerline
    fi
}

install_powerline
source bootstrap.sh
