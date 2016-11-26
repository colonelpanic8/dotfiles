#!/usr/bin/env zsh

systemctl --user | tail -n +2 | head -n -6 |
	egrep -v "sys-devices" | get_cols 1 | rofi -dmenu -i | xargs systemctl --user restart
