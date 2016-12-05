#!/usr/bin/env zsh

systemctl --user list-unit-files | tail -n +2 | head -n -2 |
	rofi -dmenu -i | get_cols 1 | xargs systemctl --user restart
