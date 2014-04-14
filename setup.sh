#!/bin/sh
case `uname` in
    'Darwin')
        readlink_command='greadlink'
        ;;
    *)
        readlink_command='readlink'
esac
DOTFILES_DIRECTORY="$(dirname "${BASH_SOURCE}" | xargs ${readlink_command} -f)"
cd $DOTFILES_DIRECTORY

case `uname` in
    'Darwin')
        source resources/osx.sh
        source resources/brew.sh
        ;;
    'Linux')
        source resources/apt-get.sh
        ;;
esac

function install_python_packages() {
    sudo -v
    source ~/.path
    easy_install pip
    pip install -r requirements.txt
}


echo "Installing Dot Files."
source bootstrap.sh
