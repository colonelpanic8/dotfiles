#!/usr/bin/env bash

cd "$1"

echo "Syncing $1"
git-sync
enable_git_sync.sh

while changedFile=$(inotifywait ./ -r -e modify,move,create,delete --format "%w%f" --exclude '\.git' 2>/dev/null); do
	git check-ignore "$changedFile" >> /dev/null || git-sync
done
