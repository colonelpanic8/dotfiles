#!/bin/bash
# Assumes that gcc, make and git as well as a package manager (brew,
# apt-get) is installed on the system.

# Go to the source directory of this script.
cd "$(dirname "${BASH_SOURCE}")"

case `uname` in
    'Darwin')
	source .brew
	;;
    'Linux')
	source .apt-get
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
echo "Installing Tmux Configuration."
tmux-powerline/generate_conf.sh
echo "Installing oh-my-zsh."
oh-my-zsh/install.sh -f
