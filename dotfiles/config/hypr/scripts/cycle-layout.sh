#!/usr/bin/env bash
# Cycle between master and dwindle layouts
# Like XMonad's NextLayout

set -euo pipefail

CURRENT=$(hyprctl getoption general:layout -j | jq -r '.str')

if [ "$CURRENT" = "master" ]; then
    hyprctl keyword general:layout dwindle
    notify-send "Layout" "Switched to Dwindle (binary tree)"
else
    hyprctl keyword general:layout master
    notify-send "Layout" "Switched to Master (XMonad-like)"
fi
