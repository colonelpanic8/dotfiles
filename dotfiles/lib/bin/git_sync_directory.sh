#!/usr/bin/env zsh

echo "Syncing $1"
cd "$1"

counter=0
while true; do
	changedFile=$(inotifywait ./ -r -e modify,move,create,delete --format "%w%f" --exclude '\.git' -t 20 2>/dev/null)
	if [ -z "$changedFile" ]
	then
		counter=$((counter+1))
		if [ $counter -gt 4 ]; then
			git-sync
			counter=0
		else
			git-sync > /dev/null
		fi
	else
		echo "Syncing for: $changedFile"
		{ git check-ignore "$changedFile" > /dev/null; } || git-sync
	fi
done
