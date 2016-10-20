#!/usr/bin/env bash

TOP_LEVEL="$(git rev-parse --show-toplevel)"
HOOK_DIR="$TOP_LEVEL/.git/hooks"

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
SOURCE_DIR="$THIS_DIR/git-hooks"

for hook in "$SOURCE_DIR"/*; do
    filename=$(basename "$hook")
    source="$SOURCE_DIR/$filename"
    dest="$HOOK_DIR/$filename"
    # If the hook already exists, is executable, and is not a symlink
    if [ -e "$dest" ]; then
        mv "$dest" "$dest.local"
    fi
    echo "linking $source to $dest"
    ln -s  "$source" "$dest"
    chmod 755 "$dest"
done
