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

source bootstrap.sh
