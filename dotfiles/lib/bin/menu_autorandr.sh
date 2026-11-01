#!/usr/bin/env zsh

autorandr 2>&1 | desktop_dmenu -i | get_cols 1 | xargs autorandr -l
