#!/usr/bin/env sh

enable_git_sync () {
	unit_name=$(systemd-escape -p "$1" --template git-sync@.service)
	echo $unit_name
	systemctl --user enable "$unit_name"
}


cd "$HOME/.config/systemd/user/"
find * -type f | grep -v git-sync | grep -E "\.service$" | xargs -I unitname sh -c 'echo unitname && systemctl --user enable unitname'

enable_git_sync "$HOME/org"
enable_git_sync "$HOME/config"
enable_git_sync "$HOME/.password-store"
