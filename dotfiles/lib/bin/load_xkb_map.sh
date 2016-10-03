#!/usr/bin/env bash

XKBDIR="$HOME/.xkb"
XKBMAPFILE="$XKBDIR/keymap/$(hostname --fqdn)"
[[ -r "$XKBMAPFILE" ]] || XKBMAPFILE="${XKBMAPFILE%/*}/default"

echo "loading keymap from $XKBMAPFILE"

xkbcomp -I"$XKBDIR" "$XKBMAPFILE" "${DISPLAY%%.*}"

unset XKBDIR XKBMAPFILE
