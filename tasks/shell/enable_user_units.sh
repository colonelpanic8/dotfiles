#!/usr/bin/env sh

enable_git_sync () {
	[ -e "$1" ] || git clone $2 $1
	unit_name=$(systemd-escape -p "$1" --template git-sync@.service)
	echo $unit_name
	cd $1
	git config --bool branch.master.sync true
	git config --bool branch.master.syncNewFiles true
	git branch --set-upstream-to=origin/master
	systemctl --user enable "$unit_name"
	systemctl --user restart "$unit_name"
}

cd "$HOME/.config/systemd/user/"
find * -type f | grep -v git-sync | grep -E "\.service$" | xargs -I unitname sh -c 'echo unitname && systemctl --user enable unitname'

enable_git_sync "$HOME/org" git@bitbucket.org:ivanmalison/org.git
enable_git_sync "$HOME/config" git@bitbucket.org:ivanmalison/config.git
enable_git_sync "$HOME/.password-store" git@bitbucket.org:ivanmalison/pass.git
