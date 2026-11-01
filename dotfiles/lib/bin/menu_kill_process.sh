#!/usr/bin/env zsh

ps -o 'pid= cmd=' x \
  | sed -E 's@/nix/store/[[:alnum:]]{32}-@@g' \
  | rofi -dmenu -i \
  | get_cols 1 \
  | xargs kill -9
