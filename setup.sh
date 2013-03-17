#!/bin/bash
cd "$(dirname "${BASH_SOURCE}")"

source bootstrap.sh

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
	echo 'Linux'
	hash apt-get &>/dev/null || echo 'apt-get is missing.' && exit
	apt-get install build-essential
	;;
    *)
	echo "Operating System not recognized; aborting."
	exit
esac

tmux-powerline/generate_conf.sh
oh-my-zsh/tools/install.sh
chsh -s /bin/zsh
