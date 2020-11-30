#!/usr/bin/env bash

XKBDIR="$HOME/.xkb"
XKBMAPFILE="$XKBDIR/keymap/$(hostname --fqdn)"
[[ -r "$XKBMAPFILE" ]] || XKBMAPFILE="${XKBMAPFILE%/*}/default"

echo "loading keymap from $XKBMAPFILE"

xkbcomp -I"$XKBDIR" "$XKBMAPFILE" "${DISPLAY%%.*}"

XKB_DEVICE_DIR="$XKBDIR/devices"

for file in "$XKB_DEVICE_DIR"/*
do
	DEVICE_ID=$(xinput | grep "$(basename $file)" | head -n 1 | grep -Eo "id=[0-9]*" | grep -Eo "[0-9]*")
	echo $file
	echo device
	echo "$DEVICE_ID"
	# [ ! -z "$DEVICE_ID" ] && xkbcomp -I"$XKBDIR" -i "$DEVICE_ID" "$file" "${DISPLAY%%.*}"
done
