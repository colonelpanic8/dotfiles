#!/usr/bin/env zsh

#!/usr/bin/env bash

echo "Syncing $2 at $1 with a default sync interval of $3"

# Initialize the directory
if [ ! -d  "$1" ]; then
   base="$(dirname $1)"
   mkdir -p "$base"
   cd "$base"
   git clone "$2" "$(basename $1)"
fi

cd "$1"

while true; do
	changedFile=$(
		inotifywait ./ -r -e modify,move,create,delete \
			--format "%w%f" --exclude '\.git' -t $3 2>/dev/null
	)
	if [ -z "$changedFile" ]
	then
		echo "Syncing due to timeout"
		git-sync -n -s
	else
		echo "Syncing for: $changedFile"
		{ git check-ignore "$changedFile" > /dev/null; } || git-sync -n -s
	fi
done
