#!/usr/bin/env zsh

pulseaudio-ctl "$@"

volnoti-show "$(pavolume)"
