#!/usr/bin/env bash

args="$(rofi -input /dev/null -dmenu)"
sleep 1
scrot $args
