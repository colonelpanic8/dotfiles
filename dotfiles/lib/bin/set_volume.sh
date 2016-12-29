#!/usr/bin/env zsh

pulseaudio-ctl "$@"

notify-send " " -i notification-audio-volume-high -h int:value:$(SINK=1 pavolume) -h string:synchronous:volume
