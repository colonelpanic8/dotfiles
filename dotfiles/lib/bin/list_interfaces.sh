#!/usr/bin/env zsh

ip link show | grep -vE '^ ' | get_cols -F ':' 2 | xargs -n 1
