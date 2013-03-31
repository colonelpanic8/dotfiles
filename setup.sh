#!/bin/bash
# Go to the source directory of this script.
cd "$(dirname "${BASH_SOURCE}")"

function install_essentials() {
    case `uname` in
	'Darwin')
	    hash gcc &>/dev/null
	    if [ $? -ne 0 ]
	    then
		echo "gcc not found."
		exit
		case `sw_vers | grep ProductVersion | awk '{print $2}' | awk 'BEGIN{FS="."}{print $2}'` in
		    '8')
			echo 'Mountain Lion Detected.'
			;;
		    '7')
			echo 'Lion Detected.'
			;;
		    *)
			exit -1
			;;
		esac
	    else
		echo "gcc found."
		source ~/.osx
	    fi
	    hash brew &>/dev/null && echo "brew found." || ruby -e "$(curl -fsSL https://raw.github.com/mxcl/homebrew/go)"
	    source .brew
	    ;;
	'Linux')
            echo "Linux detected."
	    hash apt-get &>/dev/null || (echo 'apt-get is missing.' && exit)
            echo "apt-get found."
	    sudo apt-get install build-essential
            sudo apt-get install git
            sudo git clone git@github.com:IvanMalison/dotfiles.git
            cd dotfiles
            source .debian
	    ;;
	*)
	    echo "Operating System not recognized; aborting."
	    exit
    esac
}

function install_python_packages() {
    sudo -v
    source ~/.path
    easy_install pip
    pip install -r requirements.txt
}


sudo -v
[ "$1" == "--install" -o "$1" == "-i" ] && echo "Installing essential files." && install_essentials
echo "Installing Dot Files."
source bootstrap.sh
echo "Installing Tmux Configuration."
tmux-powerline/generate_conf.sh
echo "Installing oh-my-zsh."
oh-my-zsh/install.sh -f
