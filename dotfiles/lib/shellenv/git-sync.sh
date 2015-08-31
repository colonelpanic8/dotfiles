function git-sync {
	local target="$1"
	local destination="$2"
	cd "$destination"
	git fetch origin
	# This won't work for branches other than master
	git rebase origin/master

	rsync -rt $target/* $destination
	git add .
	git commit -am "$(date)"
	git push origin HEAD
	git clean -fd
}
